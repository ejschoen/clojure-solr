# Connection reuse and timeouts on the SolrJ 10 client

**Written 2026-08-23, from a production wedge on i2kconduit-db.** This documents a
failure the Solr 10 migration did not anticipate, the reasoning behind the fix, and the
things a later editor must not undo. It is a companion to
`i2kconduit/claude-docs/projects/solr-10-migration/the-solr-10-seam.md` — **read that
first**; it holds the design rationale for the `SolrConnection` protocol, `shared?`, and
the decision to deprecate rather than port pooling. Nothing here overturns it.

## What happened

A deployed i2kconduit-db follower stopped processing entirely and stayed that way. Every
health surface the fleet exposes read clean.

Thread dump: **all 8 worker threads** parked `WAITING`, identically:

```
HttpJdkSolrClient.requestWithBaseUrl(HttpJdkSolrClient.java:176)
  -> jdk.internal.net.http.HttpClientImpl.send -> CompletableFuture.get -> Unsafe.park
clojure_solr.impl.solr10$authenticating_client$fn__145.invoke(solr10.clj:134)
```

`netstat`: **exactly one** socket to Solr, idle — `Recv-Q 0`, `Send-Q 0` — and no
`SYN_SENT`. So the eight requests were **not waiting on the network**. Their connections
were already gone and their `CompletableFuture`s were orphaned, never completing either
way. Solr itself was healthy throughout, with memory to spare; it was never asked
anything.

`lsof`: roughly **35 `eventpoll`/`eventfd`/`timerfd` triples** against ~12 sockets. One
triple per JDK `HttpClient` selector.

The trigger was a restart of a *different* process on the Solr host. That is a red
herring worth naming, because it cost an hour: the restart did not break the Solr
connection. It perturbed one, and the missing timeout turned a transient perturbation
into a permanent wedge.

## Why one lost connection killed the process

1. The caller does `(with-connection (connect solr-url) ...)` **per document**
   (i2kconduit-db `update.clj`, `save-doc-to-solr`).
2. `with-connection` reuses only when `(shared? old)`, and `shared?` is true **only for
   `EmbeddedSolrServer`**. The HTTP client inherits the `false` default. So the HTTP path
   builds a fresh client per document — **a new TCP connection and a new HTTP/2 preface
   for every document saved**.
3. SolrJ 10 pools per client *instance*. The seam doc measured this: *"50 sequential
   queries through one client cost 4 file descriptors."* **Through one client.** Pooling
   per instance buys nothing when the instance is per operation.
4. On **Solr 9 this same call pattern still pooled**, because clojure-solr held a shared
   Apache connection manager that every per-call client borrowed. Deprecating it was the
   right call for reused clients — but per-call sites silently lost cross-call pooling in
   the migration, and nothing flagged it, because the measurement that justified the
   deprecation was taken through a reused client.
5. `jdk-client` applies `.withIdleTimeout` only `(when socket-timeout ...)`. **The default
   is wait-forever.** A blocking `HttpClient.send` with no bound is a footgun for every
   caller.
6. On **Java 17 `java.net.http.HttpClient` is not `AutoCloseable`** — that arrived in Java
   21. So `with-connection`'s `finally (.close c)` cannot release the JDK client's
   selector thread. Per-document clients leak selectors until GC, which is what those ~35
   triples are. The eight wedged threads never reached their `finally` at all.

Each preface is another roll of the dice on a connection-setup race. Get unlucky once,
and with no timeout that worker is gone permanently.

## The fix, and why it is three coupled changes

**Caching alone is wrong.** `with-connection`'s `finally` closes the client it bound. If
`connect` starts returning a cached instance, the first scope to exit destroys it for
everyone else. And the existing reuse branch fires only when an *outer* `*connection*` is
already bound, which is not the same as caching across independent calls.

So:

1. **Cache the HTTP client** per target. Key on what actually distinguishes a client —
   URL plus the options that change construction. Different options must not share.
2. **`shared?` true for cached clients** — precisely the contract the seam doc already
   states: *"a process-lifetime resource that nested scopes should reuse rather than
   rebuild."*
3. **`with-connection` closes only what it created.** Close what you made, not what you
   borrowed.
4. **A default timeout**, overridable, documented. A caller who wants to wait forever
   should have to ask.

## Things not to undo

- **The pooling deprecation is correct** for reused clients and the seam doc's numbers
  back it. This change does not restore a connection manager; it makes reuse real so the
  JDK client's own pooling applies.
- **`shared?` is about lifetime, not lifecycle protection** (seam doc). Extending it to
  cached HTTP clients is consistent with that; using it as a general "don't close me"
  flag is not.
- **Both SolrJ 8/9 and SolrJ 10 remain supported.** Consumers still on clojure-solr 6.0.0
  with SolrJ 8.11.4 must keep working.
- **`EmbeddedSolrServer` reuse silently returns the bound core** even when a nested
  `connect` names a different one. The seam doc calls this an accident of the class-name
  match that is now a stated rule. Do not let a general cache change that behaviour.

## The verification gate

From the migration README, and it is the gate that would have caught the original bugs:

> **connect twice through the real client, and run a query.**

The second connect is what exercises reuse. Note the distinction this failure turned up:
twice through *one* client is reuse; twice through *two* clients is what production
actually did, and it is the case no test covered.

Worth testing explicitly: two connects return the same instance; different options do
not; `with-connection` does not close a shared client; it does still close one it
created; the default timeout reaches the built client.

## Who depends on this

- **i2kconduit-db** — write-heavy, one Solr save per document. The wedge above.
- **i2kweb** — read-heavy. Same call pattern, more traffic; check its call sites.
- Other consumers remain on clojure-solr 6.0.0 / SolrJ 8.11.4.

Tests may need Java 21 — embedded Solr 10 is compiled to class file 65 while the
applications run 17. Use `JAVA_CMD=/usr/lib/jvm/jdk-21.0.6+7/bin/java`.

---

## Addendum, 2026-08-23: what the measurements said

Added after the fix landed, from running the JDK and the fixed library rather
than reading them. Nothing above is retracted except where marked. Harnesses are
in `claude-docs/evidence/`.

### Step 5 is wrong about the default, and it does not matter

"The default is wait-forever" is not so. `HttpSolrClientBuilderBase`
`getIdleTimeoutMillis` returns 600000 when unset, `getConnectionTimeoutMillis`
returns 60000, `getRequestTimeoutMillis` falls back to the idle timeout, and
`HttpJdkSolrClient.decorateRequest` applies it to `HttpRequest.Builder.timeout`.
An unconfigured client carries a 10-minute per-request bound.

And the bound arms before the exchange starts. With a 3 s request timeout and no
connect timeout (`TimeoutProbe.java`):

| | Java 17 | Java 21 |
|---|---|---|
| TCP connect never completes | 3173 ms, HttpConnectTimeoutException | 3185 ms |
| request sent, no reply | 3004 ms, HttpTimeoutException | 3004 ms |
| h2c upgrade accepted, SETTINGS never sent | 3002 ms, HttpTimeoutException | 3003 ms |

So the timeout was never the reason eight threads sat for 72 minutes.

### What actually swallows it

The executor. `HttpJdkSolrClient.close` shuts down the `ExecutorService` the JDK
client delivers through — and on Java 17 does so *without* closing the JDK client,
which only became `AutoCloseable` in Java 21. A timeout then has no way to arrive.

| | Java 17 | Java 21 |
|---|---|---|
| executor `shutdownNow()` mid-request | **never returns** | 3014 ms, RejectedExecutionException |
| graceful `shutdown()` mid-request | **never returns** | 3003 ms, RejectedExecutionException |
| `sendAsync().orTimeout()`, dead executor | 3013 ms | 3002 ms |

The parked stack is `CompletableFuture$Signaller.block` under
`Unsafe.park` — the production dump. Graceful `shutdown()` alone is enough.

Three consequences:

1. **Client reuse is the fix**, and not merely because it means fewer connection
   setups: it removes the `close` from the per-operation path, so no thread is
   ever holding a request on a client someone else is tearing down.
2. **The timeouts are still worth setting** — they fire whenever the executor is
   alive — but they are the seatbelt, not the airbag.
3. **This is Java-17-only.** The applications run 17; solr-core 10 forces the
   test suite onto 21, where the same sequence fails fast. No test in this repo
   can reproduce the wedge, so the rule that nothing closes a borrowed client has
   to be carried by review, not by CI.

`orTimeout` bounds even the dead-executor case, because `CompletableFuture`'s
delayer runs its own scheduler. Using it would mean driving `requestAsync`
instead of SolrJ's blocking `send`. Not done here; it is the only client-side
thing that survives a dead executor, so it is the option to reach for if one is
ever wanted.

### What the cache is worth

3200 saves, 8 threads, Java 17, SolrJ 10 against a local HTTP endpoint
(`solr_stress.clj`). Both columns do the same `(with-connection (connect url) ...)`
per operation:

| | connect per operation | cached |
|---|---|---|
| wall clock | 8098 ms | **291 ms** |
| p50 / p99 / max per op | 6.9 / 737.6 / 1563.7 ms | **0.6 / 3.8 / 6.6 ms** |
| TCP connections opened | 3206 | **10** |
| epoll descriptors held after | +1458 | **+1** |
| threads held after | +3272 | **+14** |

The p99 of 738 ms in the left column is the connection-setup race the analysis
above predicted, showing up as latency before it ever shows up as a wedge.

A soak of 32000 operations over 16 threads held steady at 21 connections, one
epoll descriptor, +24 threads, p99 2.2 ms.

### One defect the stress found

The first cache stored a `delay` and registered the client from inside it. A
concurrent `close-cached-clients!` could drop the not-yet-realized entry from the
map while the delay went on to register the client anyway — leaving a client that
reports itself cache-owned, so no scope will close it, and that the cache no
longer holds, so `close-cached-clients!` cannot reach it either. It leaks for the
lifetime of the process.

Racing the two algorithms directly (`cache_ab.clj`), 8 threads against 400
closes: the delay version leaked 172, 200 and 198 registrations on three runs;
the lock version leaked none. Building, registering and installing now happen
together under one lock that `close-cached-clients!` also takes.
`cache-owned-count` must equal `cached-client-count`, and a test asserts it.

### The wedge, reproduced end to end

Eight workers issuing one slow request each through a single client, plus one
more scope that borrows the same client, does nothing, and exits while they are
all still mid-request -- that exiting scope runs `with-connection`'s `finally`:

| | Java 17 | Java 21 |
|---|---|---|
| client built by the caller and shared | **0 completed, 8 parked forever** | 8 completed |
| client obtained from `connect` (cached) | 8 completed | 8 completed |

The parked stack is the production one. This is the reproduction the analysis
above said no test had: the ingredient is concurrency over a *shared* client with
a per-operation close, which no single-threaded test can produce.

It also bounds what this change fixes. Caching makes `(with-connection (connect
url) ...)` safe, because no scope may close a cached client. A client the caller
built and shares by hand is not cache-owned, so `with-connection` still closes
it and the wedge still happens. `with-opened-connection` is the existing answer;
routing the client through `connect` is the other.

Two corrections from the production JVM, which arrived after the fix was written
and which retract claims made earlier in this document:

- **There was no client per document.** The dumps show exactly two
  `HttpJdkSolrClient` instances and two JDK selector managers, both `RUNNABLE` in
  `EPoll.wait`, with 8 wedged workers = 2 clients x 4 executor threads. Something
  shares clients across threads, which is what made the cross-thread close the
  default case rather than a possibility.
- **There was no selector leak.** The "~35 eventpoll/eventfd/timerfd triples"
  were Reactor Netty's, from 32 `reactor-http-epoll` threads, not discarded
  HttpClients. The per-operation selector cost measured here is real for the
  pattern -- 3200 uncached operations did create 1458 epoll descriptors -- but it
  was not what that JVM was suffering from.

Neither retraction changes the fix, and both sharpen which half carries it: not
the caching, but `with-connection` closing only what it created.

### The consumer already shared its clients

Reading i2kconduit-db settled where the sharing comes from. `update.clj` does not
call clojure-solr's `connect` or `with-connection` at all -- it `:use`s
`i2kconduit-db.util`'s own (util.clj:371 and :426). That `connect` caches one
client per URL in a ref, so `save-doc-to-solr` per document is not a client per
document: it is one shared client per URL, handed to every worker. Two cached
URLs, the main one and `@solr-datapoint-url`, is exactly the two clients the dump
shows.

Until i2kconduit-db's a622059 (2026-08-20 14:16) that `with-connection` closed
unconditionally, guarded only by `solr/shared?`, which answered false for every
HTTP client. Shared client, closed on every scope exit, eight workers: the wedge.

Replaying that exact macro against the fixed library, on Java 17:

| shared client obtained by | `shared?` | result |
|---|---|---|
| `connect` with `:cache-client? false` | false | **0 completed, 8 parked forever** |
| `connect` (cached) | true | 8 completed, 0 parked |

So the library change alone neutralises the consumer's bug: `shared?` now answers
true for a cached client, and the application's *old* macro would no longer close
it. That is worth more than the caching, and it is the reason to prefer asking
`shared?` over any local notion of ownership.

Two cautions on reading the dump:

- **A closed client is invisible in a thread dump.** Its executor threads are
  gone, so the two healthy clients on display need not be the ones the wedged
  workers are holding. The gaps in the numbering -- `HttpJdkSolrClient-1` and
  `-3`, `HttpClient-1` and `-4` -- say other clients existed and went away.
- **SolrJ gives each client a 4-thread delivery pool**:
  `MDCAwareThreadPoolExecutor(4, 256, 60s, LinkedBlockingQueue(1024))`. A
  ThreadPoolExecutor only grows past its core size once the queue is full, so
  with a 1024-deep queue the pool stays at 4 under any realistic load. Four core
  threads parked untimed in `take()` is an *idle* pool, not a blocked one.

### What a cached client costs to hold

There is no eviction, so a consumer holds one client per distinct target for the
life of the process.  Measured by connecting to N targets and holding them:

| targets | file descriptors | epoll | threads |
|---|---|---|---|
| 25 | +101 | +25 | +150 |
| 100 | +393 | +100 | +596 |

About 4 descriptors and 6 threads each, linear, one epoll descriptor apiece.
Fine for a handful of collections; worth sizing for at a hundred.

### If the applications move to Java 21

No code change: `solr-solrj` 10 is class file 61, `solr-core` 10 is 65, and
production loads only the former. The 32000-operation soak above ran on Java 17.

Moving to 21 turns the wedge into a `RejectedExecutionException` and lets the
suite run on the same JVM as production, which today it cannot. One behaviour
changes, measured with a request in flight:

| | Java 17 | Java 21 |
|---|---|---|
| `close()` with a 4 s request timeout | 0 ms (skipped; not AutoCloseable) | 3018 ms |
| `close()` with no request timeout | 0 ms (skipped) | **still blocked at 15 s cap** |

On 17 SolrJ cannot close the JDK client at all, so it only shuts the executor --
instant, and it strands the in-flight thread. On 21 `HttpClient.close` waits for
outstanding requests, so the wait moves from the requesting thread to the closing
one. That makes the default `:socket-timeout` matter *more* after a migration,
not less: it is what bounds a close. With the cache, closes are rare;
`close-cached-connections!` is the call to think about.

### Retraction: the production wedge was not a close

The heap dump from the wedged pod settles it, against the theory this document
and the fix were built on. Read with MAT's indices and the NetBeans heap library:

- **Exactly two `HttpJdkSolrClient` instances, both alive.** `httpClient` and
  `executor` are non-null on both -- `close()` nulls both and nothing else does.
  The two URLs are `/solr/i2ksearch` and `/solr/i2klogs`, the application's two
  cached targets. **Nothing was ever closed, so close-under-use did not happen.**
- Both carry `requestTimeoutMillis = 600000`. The bound was there.
- **`HttpClientImpl.timeouts` is empty on both clients**, while one has
  `pendingHttpRequestCount = 10`. No timer is armed for any pending request.
  That, and not a dead executor, is why 600000 ms never fired.
- That same client has `pendingHttp2StreamCount = 0`,
  `pendingTCPConnectionCount = 0`, and both connection pools empty, and its
  `Http2ClientImpl` holds no pooled connection. Ten requests are registered as
  pending with no connection and nothing attempting one.
- An `Http2Connection` with `finalStream = true` and `nextstreamid = 1633091`
  -- roughly 816000 streams used -- sits outside every pool.

So: a long-lived HTTP/2 connection reached its final stream and was evicted, the
queued requests needed a replacement, none was started, and no deadline was armed
to break the wait. The single idle socket in `netstat` belongs to the *other*
client, which is healthy. This is a connection-replacement failure below SolrJ,
not anything clojure-solr does.

Also noted while checking: `HttpJdkSolrClient.maybeTryHeadRequestSync` builds its
HEAD probe with no `.timeout()` at all, so that probe is unbounded by
construction.

What this costs the rest of this document: the caching and the close rule remain
correct and their measurements stand, but **they would not have prevented this
outage**. The reproduction above is a real failure mode of the old code that is
now closed off; it was not this one. The only client-side bound that would have
helped is one that does not depend on the JDK's timer set -- `orTimeout` on
`requestAsync`, measured earlier at 3013 ms against a dead executor. That moves
from a footnote to the live proposal.

Immediate mitigation, needing no code change: `-Dsolr.http1=true` makes SolrJ
build the JDK client with `HttpClient.Version.HTTP_1_1`
(`defaultUseHttp1_1 = Boolean.getBoolean("solr.http1")`), which avoids the HTTP/2
connection-replacement path entirely.

### The JDK defect the heap state matches

`JDK-8385131`, "HTTP/2 connection not closed after receiving GOAWAY frame when no
streams are active". Its root cause, quoted from the report:

> when `streams.isEmpty()` -- the common case for idle pooled connections --
> `handlePeerUnprocessedStreams()` is a no-op. Since `closeStream()` is never
> invoked, the `close()` method (which is triggered when the last stream finishes
> on a `finalStream`-marked connection) is never called.

The dump holds exactly that object: `Http2Connection#2273087656`,
`finalStream=true`, `streams(in flight)=0`, belonging to the wedged client and
present in no pool. A GOAWAY marked it final, there were no streams to drive the
close, and it was left behind.

**The version window fits precisely.** `JDK-8385131` is a regression introduced by
`JDK-8335181` ("Incorrect handling of HTTP/2 GOAWAY frames in HttpClient",
fixed in 24), which was backported to **17.0.17** as `JDK-8364535`. The pod runs
**17.0.19+10** -- read out of the heap -- so it carries the change that creates
this state and not the fix, which is `fixVersion 28`.

What this does not explain on its own: `JDK-8385131` is reported as a *lingering
connection*, whereas the pod also shows ten requests pending with
`pendingTCPConnectionCount = 0` and no timer armed. The zombie connection is
established; why no replacement is attempted, and why no deadline is registered,
is not covered by that report. Treat the match as strong but partial.

**Not `JDK-8326498`.** That one is a connection *leak* -- "the number of
connections increases over time" -- reported against 21.0.2, fixed in 26, with no
17u backport in its record. The pod's netstat showed a single idle socket and its
heap shows no pooled connections at all: the opposite observable.

Mitigation is unchanged and now doubly supported, since `JDK-8335181`'s own
reporter writes "If specifically configuring the client as http/1.1 then
everything works": speak HTTP/1.1, or move to a JDK carrying the `JDK-8385131`
fix.

Prefer `connect`'s `:http1? true` to SolrJ's `solr.http1` system property.
Measured: the property is read by `Boolean.getBoolean` in the *instance*
constructor of `HttpSolrClientBuilderBase`, not in a static initialiser, so
`(System/setProperty "solr.http1" "true")` does work -- but only for clients
built afterwards.

    client built BEFORE setProperty            forceHttp11=false  HTTP_2
    client built AFTER  setProperty            forceHttp11=true   HTTP_1_1
    the earlier client, re-fetched from cache  forceHttp11=false  HTTP_2
    same URL after close-cached-connections!   forceHttp11=true   HTTP_1_1

The third line is the trap: the property is not part of the cache key, so a
client built before it was set keeps speaking HTTP/2 for the life of the
process, and the only way to rebuild it is to close it -- which on Java 17 is
the one operation that wedges threads mid-request. A `-D` flag on the command
line avoids the ordering question entirely; `:http1?` avoids it by construction.

**Do not try to recover by shutting down the client's executor.** It is a
circulating suggestion and it is the wedge, not the cure: measured here on Java
17, shutting that executor down under a live request parks the requesting thread
permanently, and a graceful `shutdown()` is enough to do it.

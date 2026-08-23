import java.io.*;
import java.net.*;
import java.net.http.*;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;

/**
 * Does java.net.http's per-request timeout bound a request that never reaches
 * the wire?  Each scenario runs on its own thread with a hard cap, so a wedge
 * shows up as "STILL PARKED" rather than hanging the probe.
 */
public class TimeoutProbe {

    static final Duration REQ_TIMEOUT = Duration.ofSeconds(3);
    static final long CAP_MS = 20_000;

    // ---- servers -----------------------------------------------------------

    /** Accepts, then never writes.  The request reaches the wire; no reply. */
    static ServerSocket blackHoleAccept() throws IOException {
        ServerSocket ss = new ServerSocket(0, 50, InetAddress.getLoopbackAddress());
        Thread t = new Thread(() -> {
            List<Socket> held = new ArrayList<>();
            try { while (true) held.add(ss.accept()); } catch (Exception ignored) {}
        });
        t.setDaemon(true); t.start();
        return ss;
    }

    /** Never accepts, and its accept queue is pre-filled, so SYNs are dropped. */
    static ServerSocket synBlackHole() throws IOException {
        ServerSocket ss = new ServerSocket(0, 1, InetAddress.getLoopbackAddress());
        List<Socket> fill = new ArrayList<>();
        for (int i = 0; i < 24; i++) {
            Socket s = new Socket();
            try { s.connect(new InetSocketAddress(InetAddress.getLoopbackAddress(), ss.getLocalPort()), 200); fill.add(s); }
            catch (Exception e) { break; }
        }
        HOLD.addAll(fill);
        return ss;
    }
    static final List<Socket> HOLD = Collections.synchronizedList(new ArrayList<>());

    /** Speaks the h2c upgrade, then stalls before sending SETTINGS. */
    static ServerSocket stalledH2Preface() throws IOException {
        ServerSocket ss = new ServerSocket(0, 50, InetAddress.getLoopbackAddress());
        Thread t = new Thread(() -> {
            List<Socket> held = new ArrayList<>();
            try {
                while (true) {
                    Socket s = ss.accept();
                    held.add(s);
                    new Thread(() -> {
                        try {
                            InputStream in = s.getInputStream();
                            byte[] buf = new byte[8192];
                            in.read(buf);                       // the upgrade request
                            OutputStream out = s.getOutputStream();
                            out.write(("HTTP/1.1 101 Switching Protocols\r\n"
                                     + "Connection: Upgrade\r\n"
                                     + "Upgrade: h2c\r\n\r\n").getBytes("US-ASCII"));
                            out.flush();
                            // and now nothing: no SETTINGS frame, ever.
                        } catch (Exception ignored) {}
                    }) {{ setDaemon(true); }}.start();
                }
            } catch (Exception ignored) {}
        });
        t.setDaemon(true); t.start();
        return ss;
    }

    // ---- harness -----------------------------------------------------------

    static void scenario(String name, Callable<String> body) {
        FutureTask<String> task = new FutureTask<>(body);
        Thread t = new Thread(task, name);
        t.setDaemon(true);
        long t0 = System.nanoTime();
        t.start();
        String result;
        try {
            result = task.get(CAP_MS, TimeUnit.MILLISECONDS);
        } catch (TimeoutException te) {
            System.out.printf("  %-34s  STILL PARKED after %d ms (cap)%n", name, CAP_MS);
            StackTraceElement[] st = t.getStackTrace();
            for (int i = 0; i < Math.min(4, st.length); i++) System.out.println("        at " + st[i]);
            return;
        } catch (Exception e) {
            result = "harness error " + e;
        }
        long ms = (System.nanoTime() - t0) / 1_000_000;
        System.out.printf("  %-34s  %6d ms  %s%n", name, ms, result);
    }

    static String outcome(Throwable t) {
        StringBuilder sb = new StringBuilder(t.getClass().getSimpleName());
        if (t.getMessage() != null) sb.append(": ").append(t.getMessage());
        if (t.getCause() != null) sb.append("  <- ").append(t.getCause().getClass().getSimpleName());
        return sb.toString();
    }

    static HttpRequest req(int port) {
        return HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + port + "/solr/select"))
                .timeout(REQ_TIMEOUT).GET().build();
    }

    public static void main(String[] args) throws Exception {
        System.out.println("java " + System.getProperty("java.version")
                + "   request timeout " + REQ_TIMEOUT.toMillis() + " ms, cap " + CAP_MS + " ms");
        System.out.println("HttpClient AutoCloseable? "
                + AutoCloseable.class.isAssignableFrom(HttpClient.class));

        int syn  = synBlackHole().getLocalPort();
        int hole = blackHoleAccept().getLocalPort();
        int h2   = stalledH2Preface().getLocalPort();

        System.out.println("\n[1] does the timer arm before a connection exists?");
        scenario("connect never completes", () -> {
            HttpClient c = HttpClient.newBuilder().build();
            try { c.send(req(syn), HttpResponse.BodyHandlers.ofString()); return "returned"; }
            catch (Throwable t) { return outcome(t); }
        });

        System.out.println("\n[2] request reaches the wire, no reply");
        scenario("no response", () -> {
            HttpClient c = HttpClient.newBuilder().build();
            try { c.send(req(hole), HttpResponse.BodyHandlers.ofString()); return "returned"; }
            catch (Throwable t) { return outcome(t); }
        });

        System.out.println("\n[3] h2c upgrade accepted, SETTINGS never sent");
        scenario("stalled h2 preface", () -> {
            HttpClient c = HttpClient.newBuilder().build();
            try { c.send(req(h2), HttpResponse.BodyHandlers.ofString()); return "returned"; }
            catch (Throwable t) { return outcome(t); }
        });

        System.out.println("\n[4] executor shut down mid-request (what close() does on 17)");
        scenario("no response + executor killed", () -> {
            ExecutorService ex = Executors.newCachedThreadPool();
            HttpClient c = HttpClient.newBuilder().executor(ex).build();
            new Thread(() -> {
                try { Thread.sleep(1000); } catch (InterruptedException ignored) {}
                ex.shutdownNow();
            }) {{ setDaemon(true); }}.start();
            try { c.send(req(hole), HttpResponse.BodyHandlers.ofString()); return "returned"; }
            catch (Throwable t) { return outcome(t); }
        });

        System.out.println("\n[5] same, but sendAsync(...).orTimeout(...)");
        scenario("orTimeout + executor killed", () -> {
            ExecutorService ex = Executors.newCachedThreadPool();
            HttpClient c = HttpClient.newBuilder().executor(ex).build();
            new Thread(() -> {
                try { Thread.sleep(1000); } catch (InterruptedException ignored) {}
                ex.shutdownNow();
            }) {{ setDaemon(true); }}.start();
            try {
                c.sendAsync(req(hole), HttpResponse.BodyHandlers.ofString())
                 .orTimeout(REQ_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS).get();
                return "returned";
            } catch (Throwable t) { return outcome(t); }
        });

        System.out.println("\n[6] orTimeout against a connection that never completes");
        scenario("orTimeout + connect never completes", () -> {
            HttpClient c = HttpClient.newBuilder().build();
            try {
                c.sendAsync(req(syn), HttpResponse.BodyHandlers.ofString())
                 .orTimeout(REQ_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS).get();
                return "returned";
            } catch (Throwable t) { return outcome(t); }
        });
    
        System.out.println("\n[7] graceful executor.shutdown() only (what ExecutorUtil does first)");
        scenario("no response + graceful shutdown", () -> {
            ExecutorService ex = Executors.newCachedThreadPool();
            HttpClient c = HttpClient.newBuilder().executor(ex).build();
            new Thread(() -> {
                try { Thread.sleep(1000); } catch (InterruptedException ignored) {}
                ex.shutdown();
            }) {{ setDaemon(true); }}.start();
            try { c.send(req(hole), HttpResponse.BodyHandlers.ofString()); return "returned"; }
            catch (Throwable t) { return outcome(t); }
        });
    }
}

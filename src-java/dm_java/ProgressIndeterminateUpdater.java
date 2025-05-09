package dm_java;

public class ProgressIndeterminateUpdater {
    long startTimeMillis;
    ProgressDrawer progressDrawer;

    Thread thread;

    public ProgressIndeterminateUpdater(ProgressDrawer progressDrawer) {
        startTimeMillis = System.currentTimeMillis();
        this.progressDrawer = progressDrawer;

        thread = new Thread(() -> {
            run();
        });
        thread.start();
    }

    private void run() {
        while(true) {
            try {
                long currentTimeMillis = System.currentTimeMillis();
                long elapsed = currentTimeMillis - startTimeMillis;
                progressDrawer.setIndeterminatePercent((float)((Math.sin(elapsed / 100.0) + 1.0) / 2.0));
                ProgressManager.redraw();
                Thread.sleep(10);
            } catch(InterruptedException e) {
                return;
            }
        }
    }

    public void stop() {
        if (thread == null) return;
        thread.interrupt();
        thread = null;
    }
}

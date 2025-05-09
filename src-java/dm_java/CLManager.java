package dm_java;

import org.armedbear.lisp.*;

public class CLManager {
    static CLManager manager;

    private Main main;
    private Interpreter interpreter;

    public CLManager(Main main) throws Exception {
        this.main = main;

        if(manager != null) {
            throw new Exception("Only one CLManager allowed.");
        }

        manager = this;
    }

    void load() {
        main.loadingIndeterminate("Loading Common-Lisp...");

        if(interpreter != null) {
            interpreter.dispose();
            interpreter = null;
        }

        if(interpreter == null) {
            interpreter = Interpreter.getInstance();
        }

        if (interpreter == null) {
            interpreter = Interpreter.createInstance();
        }

        interpreter.eval("(handler-case " +
                        "   (load \"src/ui/load-files.lsp\") " +
                        "   (error (c) (let ((trace (sys:backtrace))) (format t \"[CL-ERR] Failed loading files: ~A ~A~%\" c trace))))");
    }

    private void loadFiles3(String[] paths) {
        int i = 1;
        int count = paths.length;

        for(String path : paths) {
            System.out.println("Loading: " + path);

            LispObject result = manager.interpreter.eval("(handler-case " +
            "   (progn (load \"" + path + "\") nil) " +
            "   (error (c) (format nil \"~A\" c))))");

            if (result instanceof Nil) {
                // Success
                try {
                    final int j = i;
                    javax.swing.SwingUtilities.invokeAndWait(() -> {
                        main.loadingProgress(j, count, "Loading file " + j + "/" + count);
                    });
                    i++;
                    continue;
                } catch(Exception e) {
                    e.printStackTrace();
                }
            } else if (result instanceof SimpleString) {
                System.err.println("Loading error.\n   File: " + path + "\n  Error: " + result);
            } else {
                System.err.println("Got bad result type: " + result.getClass().getName());
            }

            main.loadingFailed();
            return;
        }

        org.armedbear.lisp.Package pkg = Packages.findPackage("MAIN");
        Symbol start = pkg.findAccessibleSymbol("START");
        Function startFunction = (Function)start.getSymbolFunction();
        startFunction.execute();
    }

    private void loadFiles2(String[] paths) {
        Thread thread = new Thread(() -> {
            loadFiles3(paths);
        });

        thread.start();
    }

    public static void loadFiles(String[] paths) {
        manager.loadFiles2(paths);
    }

    public static void loadingProgress(int step, int total) {
        manager.main.loadingProgress(step, total, "Loading Files... " + step + "/" + total);
    }

    public static void reloadLisp() {
        manager.main.showLoadingPanel();
        manager.load();
    }

    public static javax.swing.JFrame getMainFrame() {
        return manager.main.frame;
    }

    public static void loadingFailed() {
        manager.main.loadingFailed();
        manager.main.showLogs();
    }

    public static void showLogs() {
        manager.main.showLogs();
    }
}

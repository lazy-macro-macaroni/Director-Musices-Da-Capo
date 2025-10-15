package dm_java;

import javax.swing.*;

import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispInteger;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Packages;

public class Main {
    JFrame frame;
    JPanel loadingPanel;

    JProgressBar progress;
    JLabel progressText;
    JButton progressRetry;

    void loadingIndeterminate(String text) {
        progressText.setText(text);
        progress.setIndeterminate(true);
        showProgress();
    }

    void loadingProgress(int step, int total, String text) {
        progress.setIndeterminate(false);
        progressText.setText(text);
        int value = Math.max(0, Math.min(100, Math.round(100f * (float)step / (float)total)));
        progress.setValue(value);
        showProgress();
    }

    void showProgress() {
        progress.setVisible(true);
        progressText.setVisible(true);
        progressRetry.setVisible(false);
    }

    void hideProgress() {
        progress.setVisible(false);
        progressText.setVisible(false);
        progressRetry.setVisible(false);
    }

    void loadingFailed() {
        progressText.setText("Loading failed.");
        progressText.setVisible(true);
        progressRetry.setVisible(true);
    }

    public static Main testLisp() {
        return null;
    }

    private static Interpreter interpreter;

    private static void loadInterpreter(Boolean showLoading) {
        if (showLoading) {
            ProgressManager.setPercentage("Loading Common-Lisp", 0);
        }
        interpreter = Interpreter.createInstance();
    }

    private static boolean globalsLoaded = false;

    private static void loadGlobals() {
        if (globalsLoaded) return;

        LispObject result = interpreter.eval(
            "(handler-case " +
            "  (progn (load \"src/globals.lsp\") 1)" +
            "  (error (c) (format t \"[CL-ERR] Failed loading globals.lsp: ~A~%\" c) 0))"
        );

        if (result instanceof LispInteger) {
            if (!result.eql(1)) {
                System.exit(1);
            }

            globalsLoaded = true;
        } else {
            System.out.println("Result is unexpectedly not integer. Program will exit.");
            System.exit(1);
        }
    }

    private static void loadFile(String fileName) {
        loadGlobals();

        LispObject result = interpreter.eval(
            "(globals:handle-errors (load \"" + fileName + "\") :success 1 :failure 0)"
        );

        if (!(result instanceof LispInteger && result.eql(1))) {
            System.out.println("Bad response loading file: " + fileName);
            System.exit(1);
        }
    }

    private static void runFunction(String packageName, String functionName, LispObject[] args) {
        try {
            packageName = packageName.toUpperCase();
            functionName = functionName.toUpperCase();

            org.armedbear.lisp.Package pkg = Packages.findPackage(packageName);

            if (pkg == null) {
                System.err.println("Couldn't find lisp package: \"" + packageName + "\".");
                System.exit(1);
            }

            org.armedbear.lisp.Symbol start = pkg.findAccessibleSymbol(functionName);

            if (start == null) {
                System.err.println("Couldn't find lisp function: \"" + functionName + "\".");
                System.exit(1);
            }

            org.armedbear.lisp.Function function = (org.armedbear.lisp.Function)start.getSymbolFunction();
            function.execute(args);
        } catch(Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private static void runFunction(String packageName, String functionName) {
        runFunction(packageName, functionName, new LispObject[]{});
    }

    public static void main(String[] argv) throws Exception {
        if (argv.length == 0 || (argv.length == 1 &&  "--run".equals(argv[0]))) {
            loadInterpreter(true);
            ProgressManager.setPercentage("Loading...", 0);
            loadFile("src/init.lsp");
            if (LoadingError.ErrorHappened) System.exit(1);
            ProgressManager.setPercentage("Starting...", 1);
            runFunction("main", "main");
            return;
        }

        if ((argv.length == 1 || argv.length == 2) && "--build".equals(argv[0])) {
            int option = -1;

            if (argv.length == 2) {
                try {
                    option = Integer.valueOf(argv[1]);
                } catch (NumberFormatException e) {
                    System.out.println("Is not valid integer: " + argv[1]);
                    System.exit(1);
                }
            }

            System.out.print("Loading lisp interpreter...");
            loadInterpreter(false);
            System.out.println("OK");
            loadFile("src-build/build-init.lsp");
            if (LoadingError.ErrorHappened) System.exit(1);
            runFunction("build-main", "main", new LispObject[]{ LispInteger.getInstance(option) });
            return;
        }

        if (argv.length == 1 && "--help".equals(argv[0])) {
            System.out.println(
                "\nDirector-Musices Arguments:\n" +
                "\n  --help     Print this help." +
                "\n  --build    Run build script." +
                "\n  --build n  Run build script with <n> option selected." +
                "\n  --run      Run program. Default option. (Same as \"--build 1\", but faster)" +
                "\n"
            );
            return;
        }

        if (argv.length == 1 && "--test".equals(argv[0])) {
            System.out.print("Loading lisp interpreter...");
            loadInterpreter(false);
            System.out.println("OK");
            loadFile("src-test/test-main.lsp");
            if (LoadingError.ErrorHappened) System.exit(1);
            runFunction("test-main", "main");
            return;
        }

        System.out.println("Bad arguments. Run with --help for help.");
    }
}

package dm_java;

// import org.armedbear.lisp.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispInteger;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Packages;

import java.awt.image.BufferedImage;
import java.io.File;
// import java.nio.file.Paths;

import javax.imageio.ImageIO;

public class Main {
    public class OnCloseListener extends WindowAdapter {
        @Override public void windowClosing(WindowEvent e) {
            if(clManager == null) { return; }
            // clManager.close();
        }
    }

    JFrame frame;
    JPanel loadingPanel;
    LogsFrame logsFrame;

    JProgressBar progress;
    JLabel progressText;
    JButton progressRetry;

    CLManager clManager;

    public Main() throws Exception {
        frame = new JFrame();
        logsFrame = new LogsFrame(frame);

        frame.addWindowListener(new OnCloseListener());

        frame.setTitle("Director Musices 2024");
        frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);

        CalculateWindowSize.setSpecs(frame, 0f, 0f, 0f, 0.303f);

        createLoadingPanel();
        showLoadingPanel();
        frame.setVisible(true);

        clManager = new CLManager(this);
        clManager.load();
    }

    void createLoadingPanel() {
        loadingPanel = new JPanel();
        loadingPanel.setLayout(new GridBagLayout());

        JPanel panel = new JPanel();
        loadingPanel.add(panel);
        BoxLayout layout = new BoxLayout(panel, BoxLayout.Y_AXIS);
        panel.setLayout(layout);

        try {
            BufferedImage pic = ImageIO.read(new File("resources/dm_transparent.png"));
            float imageScale = 0.5f;
            Image pic2 = pic.getScaledInstance((int)(pic.getWidth() * imageScale), (int)(pic.getHeight() * imageScale), Image.SCALE_DEFAULT);
            JLabel picLabel = new JLabel(new ImageIcon(pic2));
            picLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
            panel.add(picLabel);
        } catch(java.io.IOException e) {
            e.printStackTrace();
        }

        panel.add(Box.createVerticalStrut(50));

        JLabel title = new JLabel("Director Musices 2024");
        title.setAlignmentX(Component.CENTER_ALIGNMENT);
        title.setFont(new Font("Serif", Font.PLAIN, 40));
        panel.add(title);

        progress = new JProgressBar();
        progress.setValue(50);
        progress.setVisible(false);
        panel.add(Box.createVerticalStrut(30));
        panel.add(progress);

        progressText = new JLabel("");
        progressText.setAlignmentX(Component.CENTER_ALIGNMENT);
        progressText.setVisible(false);
        panel.add(Box.createVerticalStrut(15));
        panel.add(progressText);

        progressRetry = new JButton("Retry");
        progressRetry.setVisible(false);
        progressRetry.setAlignmentX(Component.CENTER_ALIGNMENT);
        panel.add(Box.createVerticalStrut(15));
        panel.add(progressRetry);
        progressRetry.addActionListener((e) -> {
            SwingUtilities.invokeLater(() -> {
                clManager.load();
            });
        });

        loadingIndeterminate("Loading...");
    }

    void showLoadingPanel() {
        // frame.setContentPane(loadingPanel);
        frame.getContentPane().add(loadingPanel, BorderLayout.CENTER);
    }

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

    void showLogs() {
        logsFrame.showLogs();
    }

    public static Main testLisp() {
        return null;
    }

    private static Interpreter interpreter;

    private static void loadInterpreter(Boolean showLoading) {
        if (showLoading) {
            ProgressManager.setIndeterminate("Loading Common-Lisp");
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

        // interpreter.eval(
        //     " (defmacro handle-errors (&rest forms)" +
        //     "  `(let ((filename \"\")" +
        //     "         (backtrace \"\"))" +
        //     "     (handler-case" +
        //     "       (handler-bind ((condition (lambda (c)" +
        //     "                                   (setf filename (if (fboundp 'get-current-file-name) (get-current-file-name) \"Unknown\"))" +
        //     "                                   (setf backtrace \"\")" +
        //     "                                   (loop for bt in (sys:backtrace) do (setf backtrace (concatenate 'string backtrace (print-trace-line (jcall \"toLispString\" bt))))))))" +
        //     "          (progn ,@forms) 1)" +
        //     "       (error (c)" +
        //     "           (jcall \"print\" (jfield \"java.lang.System\" \"err\") (format nil \"~%Error: ~A~%  In file: ~A~%  Backtrace:~%~A\" c filename backtrace)) 0))))"
        // );

        LispObject result = interpreter.eval(
            "(globals:handle-errors (load \"" + fileName + "\") :success 1 :failure 0)"
            // "(handler-case " +
            // "  (progn (load \"" + fileName + "\") 1)" +
            // "  (error (c) (let ((trace (sys:backtrace))) (format t \"[CL-ERR] Failed loading \\\"" + fileName + "\\\": ~A ~A~%\" c trace) 0)))"
        );
        // LispObject result = interpreter.eval(
        //     "(handler-case " +
        //     "  (progn (load \"" + fileName + "\") 1)" +
        //     "  (error (c) (let ((trace (sys:backtrace))) (format t \"[CL-ERR] Failed loading \\\"" + fileName + "\\\": ~A ~A~%\" c trace) 0)))"
        // );

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
        // LoadingError.Error(true, "Hello world!", "blabla\nsodifjsodif\noasdifj\ndddddd");
        // System.exit(0);

        if (argv.length == 0 || (argv.length == 1 &&  "--run".equals(argv[0]))) {
            loadInterpreter(true);
            ProgressManager.setIndeterminate("Loading Init File");
            loadFile("src/init.lsp");
            if (LoadingError.ErrorHappened) System.exit(1);
            ProgressManager.setIndeterminate("Running Init");
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

        System.out.println("Bad arguments. Run with --help for help.");

        // OLD CODE

        // if (argv.length == 1) {
        //     switch(argv[0]) {
        //         case "--build": {
        //         }
        //         case "--help": {
        //             break;
        //         }

        //         default: {
        //             System.out.println("Bad argument: " + argv[0] + "\nRun with --help for help.");
        //             break;
        //         }
        //     }
        // } else if(argv.length > 1) {

        // } else {
        //     loadInterpreter();
        //     ProgressManager.setIndeterminate("Loading Init File");
        //     loadFile("src/init.lsp");
        //     ProgressManager.setIndeterminate("Running Main");
        //     runFunction("init", "init");
        // }

        // for (String arg : argv) {
        //     System.out.println("Arg = " + arg);
        // }
        // System.out.println("Argv = " + argv);
        // Progressbar.main(argv);

        // ProgressManager.setIndeterminate("Loading Common-Lisp");

        // interpreter = Interpreter.createInstance();

        // ProgressManager.setIndeterminate("Loading Init File");

        // interpreter.eval("(handler-case " +
        //                  "  (load \"src/init.lsp\") " +
        //                  "  (error (c) (let ((trace (sys:backtrace))) (format t \"[CL-ERR] Failed loading init.lsp: ~A ~A~%\" c trace))))");

        // org.armedbear.lisp.Package pkg = Packages.findPackage("INIT");
        // org.armedbear.lisp.Symbol start = pkg.findAccessibleSymbol("INIT");
        // org.armedbear.lisp.Function startFunction = (org.armedbear.lisp.Function)start.getSymbolFunction();
        // startFunction.execute();
    }
}

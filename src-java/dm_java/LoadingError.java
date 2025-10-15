package dm_java;

public final class LoadingError {
    private LoadingError() {}

    public static boolean ErrorHappened = false;

    public static void Error(boolean showUI, String message, String longMessage) {
        ErrorHappened = true;

        ProgressManager.hide();

        if (showUI) {
            ErrorDialog(message, longMessage);
        }
    }

    private static void ErrorDialog(String message, String longMessage) {
        ErrorDialog.ShowError("Loading Error!", "Failed loading files.", message, longMessage);
    }
}

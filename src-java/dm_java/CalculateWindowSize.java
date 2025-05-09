package dm_java;

import java.awt.*;
import javax.swing.*;

public class CalculateWindowSize {
    public static float screenWidthUsed = 0.8f;
    public static float screenHeightUsed = 0.8f;

    public static class WindowSpecs {
        public final Point Position;
        public final Dimension Size;

        public WindowSpecs(Point position, Dimension size) {
            this.Position = position;
            this.Size = size;
        }
    }

    public static Dimension getScreenSize() {
        GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        return new Dimension(gd.getDisplayMode().getWidth(), gd.getDisplayMode().getHeight());
    }

    public static WindowSpecs calculateSpecs(float marginTop, float marginLeft, float marginBottom, float marginRight) {
        Dimension screenSize = getScreenSize();

        int width1 = (int)(screenSize.width * screenWidthUsed);
        int height1 = (int)(screenSize.height * screenHeightUsed);

        Insets insets = new Insets((int)(height1 * marginTop), (int)(width1 * marginLeft), (int)(height1 * marginBottom), (int)(width1 * marginRight));

        int width = width1 - insets.left - insets.right;
        int height = height1 - insets.top - insets.bottom;

        int x = (screenSize.width - width1) / 2 + insets.left;
        int y = (screenSize.height - height1) / 2 + insets.top;

        return new WindowSpecs(new Point(x, y), new Dimension(width, height));
    }

    public static void setSpecs(JFrame frame, float marginTop, float marginLeft, float marginBottom, float marginRight) {
        WindowSpecs specs = calculateSpecs(marginTop, marginLeft, marginBottom, marginRight);
        frame.setLocation(specs.Position);
        frame.setSize(specs.Size);
    }

    public static void setSpecs(JDialog dialog, float marginTop, float marginLeft, float marginBottom, float marginRight) {
        WindowSpecs specs = calculateSpecs(marginTop, marginLeft, marginBottom, marginRight);
        dialog.setLocation(specs.Position);
        dialog.setSize(specs.Size);
    }
}

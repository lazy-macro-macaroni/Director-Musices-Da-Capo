package dm_java;

import java.awt.*;
// import javax.swing.*;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Paths;

import javax.imageio.ImageIO;

public class ProgressDrawer {
    public enum ProgressType {
        INDETERMINATE,
        PERCENT
    }

    private ProgressType progressType = ProgressType.INDETERMINATE;
    private float percent = 0;
    private String text = "";

    final Color background = new Color(137, 137, 137); // Color.BLACK; // new Color(62, 6, 6); // Color.WHITE;
    final Color foreground = new Color(50, 50, 255);// new Color(26, 125, 226); // Color.WHITE; // new Color(0.3f, 0.3f, 1f);
    final Color textAlt = Color.WHITE; //new Color(0.3f, 0.3f, 1f);

    int width = 0;
    int height = 0;
    float indeterminateWidth = 0.2f;

    Rectangle backgroundRect;
    Rectangle barRect;

    private BufferedImage backgroundImage;
    private BufferedImage progressBarImage;

    public ProgressDrawer(BufferedImage backgroundImage) {
        this.backgroundImage = backgroundImage;

        var path = Paths.get(".", "resources", "dm_loading_bar.png");

        try {
            progressBarImage = ImageIO.read(path.toFile());
        } catch(IOException e) {
            System.err.println("Couldn't load image: " + path + "\nWill now exit.");
            System.exit(1);
        }
    }

    private void drawPercentText(Graphics2D g) {
        var font = Fonts.GetRoboto().deriveFont(18f);

        FontMetrics metrics = g.getFontMetrics(font);

        int strWidth = metrics.stringWidth(text);
        double x = backgroundImage.getWidth() / 2.0 - strWidth / 2.0;
        double y = 586;

        g.setFont(font);

        g.setColor(Color.black);
        g.drawString(text, (int)x, (int)y);
    }

    private void draw2(Graphics2D g) {
        var t = g.getTransform();

        try {
            g.drawImage(backgroundImage, 0, 0, null);

            g.translate(26, 438);

            g.clipRect(0, 0, Math.round(progressBarImage.getWidth() * this.percent), progressBarImage.getHeight());
            g.drawImage(progressBarImage, 0, 0, null);
            g.setClip(null);
        } finally {
            g.setTransform(t);
        }

        drawPercentText(g);
    }

    public void draw(Graphics g, int width, int height) {
        if (g instanceof Graphics2D) {
            Graphics2D g2 = (Graphics2D) g;
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            draw2(g2);
        }
    }

    public void setIndeterminatePercent(float percent) {
        this.percent = Math.max(0f, Math.min(1f, percent));
    }

    public void setPercentage(String text, float percent) {
        progressType = ProgressType.PERCENT;
        this.text = text;
        this.percent = Math.max(0f, Math.min(1f, percent));
    }

    public void setIndeterminate(String text) {
        progressType = ProgressType.INDETERMINATE;
        this.text = text;
    }
}

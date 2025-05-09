package dm_java;

import java.awt.*;
// import javax.swing.*;

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

    private void drawPercentText(Graphics g) {
        g = g.create();

        var font = Fonts.GetFreeSansBold().deriveFont(18f);

        FontMetrics metrics = g.getFontMetrics(font);
        double x = backgroundRect.x + (backgroundRect.width - metrics.stringWidth(text)) / 2.0;
        double y = backgroundRect.y + ((backgroundRect.height - metrics.getHeight()) / 2.0) + metrics.getAscent();
        g.setFont(font);

        // String percentText = "" + Math.round(percent * 100) + "%";
        g.setColor(foreground);
        g.drawString(text, (int)x, (int)y + 1);
        g.clipRect(barRect.x, barRect.y, barRect.width, barRect.height);
        g.setColor(background);
        g.drawString(text, (int)x, (int)y + 1);
    }

    private void draw2(Graphics g) {
        g.setColor(background);
        g.fillRect(backgroundRect.x, backgroundRect.y, backgroundRect.width, backgroundRect.height);

        g.setColor(foreground);
        g.fillRect(barRect.x, barRect.y, barRect.width, barRect.height);

        drawPercentText(g);
    }

    public void draw(Graphics g, int width, int height) {
        if (g instanceof Graphics2D) {
            Graphics2D g2 = (Graphics2D) g;
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        }

        this.width = width;
        this.height = height;

        // backgroundRect = new Rectangle(0, height - 25, width, 25);
        backgroundRect = new Rectangle(52, 481, 496, 28);

        switch(progressType) {
            case PERCENT: {
                barRect = new Rectangle(backgroundRect.x, backgroundRect.y, Math.round(backgroundRect.width * percent), backgroundRect.height);
                break;
            }
            case INDETERMINATE: {
                int x = Math.round(backgroundRect.x + (percent * (backgroundRect.width * (1 - indeterminateWidth))));
                barRect = new Rectangle(x, backgroundRect.y, Math.round(backgroundRect.width * indeterminateWidth), backgroundRect.height);
                break;
            }
        }

        draw2(g);
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

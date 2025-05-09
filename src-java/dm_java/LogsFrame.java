package dm_java;

import java.awt.*;
import javax.swing.*;

import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.IOException;

public class LogsFrame extends JDialog {
    public class LogsStream extends OutputStream {
        final Color color;
        final OutputStream defaultStream;
        final StringBuilder sb = new StringBuilder();

        public LogsStream(Color color, OutputStream defaultStream) {
            this.color = color;
            this.defaultStream = defaultStream;
        }

        @Override
        public void write(int b) throws IOException {
            defaultStream.write(b);

            if (b == '\r') { return; }

            if (b == '\n') {
                final String text = sb.toString() + "\n";

                appendText(text, color);
                sb.setLength(0);
            } else {
                sb.append((char)b);
            }
        }
    }

    final JTextPane textPane;

    public LogsFrame(JFrame parent) {
        super(parent);

        textPane = new JTextPane();
        textPane.setEditable(false);
        // JPanel noWrapPanel = new JPanel(new BorderLayout());
        // noWrapPanel.add(textPane);

        System.setOut(new PrintStream(new LogsStream(Color.BLACK, System.out)));
        System.setErr(new PrintStream(new LogsStream(Color.RED, System.err)));

        // try {
        //     File font_file = new File("resources/freefont/FreeMono.ttf");
        //     Font font = Font.createFont(Font.TRUETYPE_FONT, font_file);
        //     font = font.deriveFont(14f);
        //     textPane.setFont(font);
        // } catch(Exception e) {
        //     e.printStackTrace();
        // }

        setTitle("Logs");
        setDefaultCloseOperation(javax.swing.JFrame.HIDE_ON_CLOSE);
        CalculateWindowSize.setSpecs(this, 0f, 0.7f, 0f, 0f);
        setLayout(null);
        JScrollPane sp = new JScrollPane(textPane); //noWrapPanel);
        setContentPane(sp);
        setVisible(true);
    }

    private void _appendText(String text, Color color) {
        textPane.setEditable(true);
        StyleContext sc = StyleContext.getDefaultStyleContext();
        AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground, color);
        aset = sc.addAttribute(aset, StyleConstants.FontFamily, "Lucida Console");
        aset = sc.addAttribute(aset, StyleConstants.Alignment, StyleConstants.ALIGN_JUSTIFIED);

        int len = textPane.getDocument().getLength();
        textPane.setCaretPosition(len);
        textPane.setCharacterAttributes(aset, false);
        textPane.replaceSelection(text);
        textPane.setEditable(false);
    }

    public void appendText(String text, Color color) {
        SwingUtilities.invokeLater(() -> {
            _appendText(text, color);
        });
    }

    public void showLogs() {
        setVisible(true);
    }
}

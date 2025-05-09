package dm_java;

import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URI;

import javax.swing.JFrame;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

public class Progressbar {
    public static void main(String[] args) {
        final JProgressBar jProgressBar = new JProgressBar();
        jProgressBar.setMaximum(100000);
        JFrame frame = new JFrame();
        frame.setContentPane(jProgressBar);
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setSize(300, 70);
        frame.setVisible(true);

        Runnable updatethread = new Runnable() {
            public void run() {
                try {
                    URL url = URI.create("https://api.adoptium.net/v3/binary/latest/17/ga/windows/x64/jdk/hotspot/normal/eclipse?project=jdk").toURL();
                    HttpURLConnection httpConnection = (HttpURLConnection) (url.openConnection());
                    long completeFileSize = httpConnection.getContentLength();

                    java.io.BufferedInputStream in = new java.io.BufferedInputStream(httpConnection.getInputStream());
                    java.io.FileOutputStream fos = new java.io.FileOutputStream("jdk.zip");
                    java.io.BufferedOutputStream bout = new BufferedOutputStream(fos, 1024);

                    byte[] data = new byte[1024];
                    long downloadedFileSize = 0;
                    int x = 0;

                    while ((x = in.read(data, 0, 1024)) >= 0) {
                        downloadedFileSize += x;

                        // calculate progress
                        final int currentProgress = (int) ((((double)downloadedFileSize) / ((double)completeFileSize)) * 100000d);

                        // update progress bar
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                jProgressBar.setValue(currentProgress);
                            }
                        });

                        bout.write(data, 0, x);
                    }

                    bout.close();
                    in.close();

                    frame.setVisible(false);
                } catch (FileNotFoundException e) {
                } catch (IOException e) {
                }
            }
        };

        new Thread(updatethread).start();
    }
}

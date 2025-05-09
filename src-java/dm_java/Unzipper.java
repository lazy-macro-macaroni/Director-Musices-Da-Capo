package dm_java;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.nio.channels.FileChannel;

public class Unzipper {
    public Unzipper(File zipfile, File folder) throws Exception {
        FileInputStream fileInput = new FileInputStream(zipfile.getCanonicalFile());
        FileChannel channel = fileInput.getChannel();
        ZipInputStream zipInput = new ZipInputStream(new BufferedInputStream(fileInput));
        ZipEntry ze = null;

        try {
            while ((ze = zipInput.getNextEntry()) != null) {
                File f = new File(folder.getCanonicalPath(), ze.getName());

                if (ze.isDirectory()) {
                    f.mkdirs();
                    continue;
                }

                f.getParentFile().mkdirs();
                OutputStream fos = new BufferedOutputStream(new FileOutputStream(f));

                try {
                    try {
                        final byte[] buf = new byte[1024];
                        int bytesRead;
                        long length = zipfile.length();

                        while (-1 != (bytesRead = zipInput.read(buf))){
                            fos.write(buf, 0, bytesRead);
                            System.out.println("Progress = " + (1.0 * channel.position() / length));
                        }
                    } finally {
                        fos.close();
                    }
                } catch (final IOException ioe) {
                    f.delete();
                    throw ioe;
                }
            }
        } finally {
            zipInput.close();
        }
    }
}

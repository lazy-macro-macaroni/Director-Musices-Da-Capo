
package dm_java;

import java.util.Arrays;
import java.awt.*;
import javax.swing.*;

class ApiHelpers {
    private ApiHelpers() {}

    public static void JFrameSetIcons(JFrame frame, Image[] images) {
        java.util.List<Image> list = Arrays.asList(images);
        frame.setIconImages(list);
    }
}

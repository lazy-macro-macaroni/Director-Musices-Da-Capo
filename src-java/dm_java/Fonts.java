package dm_java;

import java.awt.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;

public class Fonts {
    private static HashMap<String, Font> loadedFonts = new HashMap<String, Font>();

    private static Font LoadFont(String name, Path file) {
        if (!loadedFonts.containsKey(name)) {
            try {
                Font font = Font.createFont(Font.TRUETYPE_FONT, file.toFile()).deriveFont(14f);
                loadedFonts.put(name, font);
                return font;
            } catch(Exception e) {
                e.printStackTrace();
                System.err.println("Program will exit. Couldn't load font: " + file);
                System.exit(1);
                return null;
            }
        }

        return loadedFonts.get(name);
    }

    public static Font GetFreeSans() {
        return LoadFont("FreeSans", Paths.get(".", "resources", "freefont", "FreeSans.ttf"));
    }

    public static Font GetFreeSansBold() {
        return LoadFont("FreeSansBold", Paths.get(".", "resources", "freefont", "FreeSansBold.ttf"));
    }
}

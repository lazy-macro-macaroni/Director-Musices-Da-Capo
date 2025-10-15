package dm_java;

// Helper class for creating a JTree from common lisp.
// See: src/swing/tree.lsp

public class TreeNodeInfo {
    private String title;
    private String id;

    public TreeNodeInfo(String title, String ID) {
        this.title = title;
        this.id = ID;
    }

    public String toString() {
        return title;
    }

    public String getTitle() {
        return title;
    }

    public String getID() {
        return id;
    }
}

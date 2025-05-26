/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.controllers.general;

import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.List;
import pro.metaboanalyst.utils.DataUtils;


@ViewScoped
@Named("graphStyleBean")
public class GraphStyleBean implements Serializable {
    
    @Inject
    private SessionBean1 sb;
    @Inject
    private GraphBean graphBean;
    private String selectedStyle = "default";
    // List of available styles for <ui:repeat>
    private final List<StyleItem> styleList = List.of(
            new StyleItem("default", "Default", "/resources/images/styles/default_icon.png"),
            new StyleItem("classic", "Classic", "/resources/images/styles/classic_icon.png"),
            new StyleItem("dark", "Dark", "/resources/images/styles/dark_icon.png"),
            new StyleItem("pub", "Publication", "/resources/images/styles/pub_icon.png")
    );

    public List<StyleItem> getStyleList() {
        return styleList;
    }

    public String getSelectedStyle() {
        return selectedStyle;
    }

    /**
     * user clicked a tile
     */
    public void selectStyle(String code) {
        selectedStyle = code;
    }

    /**
     * thumbnail for the larger preview pane
     */
    public String getPreviewImage() {
        return "/resources/images/styles/preview_" + selectedStyle + ".png";
    }

    /**
     * apply + redraw R figure
     */
    public void applyStyle() {
        //sb.setStyleOpt(selectedStyle);  // or however you store it
        //graphBean.regenerateCurrentFigure();      // wrap your existing logic
    }

    /* simple record */
    public static class StyleItem implements Serializable {

        public final String code, label, icon;

        public StyleItem(String c, String l, String i) {
            code = c;
            label = l;
            icon = i;
        }

        public String getCode() {
            return code;
        }

        public String getLabel() {
            return label;
        }

        public String getIcon() {
            return icon;
        }
    }
}

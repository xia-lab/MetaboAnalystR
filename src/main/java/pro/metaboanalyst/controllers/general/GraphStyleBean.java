/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.controllers.general;

import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.Serializable;
import java.util.List;
import pro.metaboanalyst.controllers.stats.UnivBean;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.utils.DataUtils;

@SessionScoped
@Named("graphStyleBean")
public class GraphStyleBean implements Serializable {

    @Inject
    private SessionBean1 sb;
    @Inject
    private GraphBean graphBean;

    @Inject
    private ApplicationBean1 ab;

    private String selectedStyle = "default";
    // List of available styles for <ui:repeat>
    private final List<StyleItem> styleList = List.of(
            new StyleItem("0", "Default", "/resources/images/styles/default_icon.png"),
            new StyleItem("1", "Classic", "/resources/images/styles/classic_icon.png"),
            new StyleItem("2", "Dark", "/resources/images/styles/dark_icon.png"),
            new StyleItem("3", "Publication", "/resources/images/styles/pub_icon.png")
    );
    private String previewImage;

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

    public String iconPath(String code) {
        // e.g. volcano  →  /resources/images/volcano_icons/graph_0.png
        String plotSrc = sb.getImgSource();                    // "volcano", "heatmap", …
        String path = ab.getRootContext() + "/resources/images/plot_icons/" + plotSrc + "/icon" + code + ".png";
        System.out.println("=====iconPath==" + path);
        return path;
    }

    /**
     * thumbnail for the larger preview pane
     */
    public String getPreviewImage() {
        String plotSource = sb.getImgSource();
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(plotSource) + "dpi72.png";
    }

    /**
     * apply + redraw R figure
     */
    public void applyStyle() {
        String plotSource = sb.getImgSource();
        System.out.println("plotSource==" + plotSource);
        if (plotSource.equals("volcano")) {
            UnivBean ub = (UnivBean) DataUtils.findBean("univBean");

            if (Integer.parseInt(selectedStyle) == 0) {
                if (ub.getLabelOpt().equals("all")) {
                    UniVarTests.plotVolcano(sb, sb.getNewImage("volcano"), ub.getPlotLbl(), ub.getPlotTheme(), "png", 72, -1);
                } else {
                    UniVarTests.plotVolcano(sb, sb.getNewImage("volcano"), ub.getPlotLbl(), ub.getPlotTheme(), "png", 72, sb.getVolcanoLabelNum());
                }
            } else {
                if (ub.getLabelOpt().equals("all")) {
                    UniVarTests.plotVolcanoCustom(sb, sb.getNewImage("volcano"), ub.getPlotLbl(), ub.getPlotTheme(), "png", 72, -1, Integer.parseInt(selectedStyle));
                } else {
                    UniVarTests.plotVolcanoCustom(sb, sb.getNewImage("volcano"), ub.getPlotLbl(), ub.getPlotTheme(), "png", 72, sb.getVolcanoLabelNum(), Integer.parseInt(selectedStyle));
                }

            }
        }
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

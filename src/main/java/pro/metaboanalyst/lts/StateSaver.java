/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import jakarta.annotation.PostConstruct;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.context.SessionScoped;
import jakarta.enterprise.inject.spi.CDI;
import jakarta.inject.Named;
import java.io.Serializable;

import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.enrich.IntegProcessBean;
import pro.metaboanalyst.controllers.enrich.IntegResBean;
import pro.metaboanalyst.controllers.enrich.MsetBean;
import pro.metaboanalyst.controllers.enrich.PathBean;

import jakarta.inject.Named;

import java.io.IOException;

import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.List;

import jakarta.inject.Inject;

import pro.metaboanalyst.utils.UtilsBean;
import pro.metaboanalyst.controllers.metapath.MetaPathStatBean;
import pro.metaboanalyst.controllers.mnet.MnetResBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;
import pro.metaboanalyst.controllers.stats.PowerAnalBean;

import pro.metaboanalyst.utils.DataUtils;

import pro.metaboanalyst.controllers.stats.RocAnalBean;
import pro.metaboanalyst.spectra.SpectraControlBean;
import pro.metaboanalyst.spectra.SpectraParamBean;

import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.controllers.meta.MetaLoadBean;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.controllers.mummichog.PeakUploadBean;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.spectra.TandemMSBean;
import pro.metaboanalyst.workflows.DiagramView;
import pro.metaboanalyst.workflows.WorkflowBean;
import pro.metaboanalyst.workflows.WorkflowView;

/**
 * @author zgy
 */
@SessionScoped
@Named("stateSaver")
public class StateSaver implements Serializable {
    @Inject
    private FireBaseController fb;

    @PostConstruct
    private void init() {
        // quick sanity check
    }

    private <T> T bean(Class<T> type) {
        return CDI.current().select(type).get();
    }

    public void saveState() {

    PeakUploadBean pub = bean(PeakUploadBean.class);
    SessionBean1 sb = bean(SessionBean1.class);
    
    HistoryBean hb = bean(HistoryBean.class);
    WorkflowBean wb = bean(WorkflowBean.class);

    MultifacBean mfb = bean(MultifacBean.class);
    MsetBean mstb = bean(MsetBean.class);
    UtilsBean utb = bean(UtilsBean.class);
    DiagramView dv = bean(DiagramView.class);

    // ––– spectra –––––––––––––––––––––––––––––––––––––––––
    SpectraProcessBean sppb = bean(SpectraProcessBean.class);
    SpectraControlBean spcb = bean(SpectraControlBean.class);
    SpectraParamBean spmb = bean(SpectraParamBean.class);

    // ––– integration / pathway ––––––––––––––––––––––––––
    IntegProcessBean itpb = bean(IntegProcessBean.class);
    IntegResBean itrb = bean(IntegResBean.class);
    PathBean patb = bean(PathBean.class);

    // ––– meta-analysis ––––––––––––––––––––––––––––––––––
    MetaPathStatBean mpsb = bean(MetaPathStatBean.class);
    MetaPathLoadBean mplb = bean(MetaPathLoadBean.class);
    MetaLoadBean mlb = bean(MetaLoadBean.class);

    // ––– single-analysis beans ––––––––––––––––––––––––––
    DoseResponseBean drb = bean(DoseResponseBean.class);
    RocAnalBean rab = bean(RocAnalBean.class);
    PowerAnalBean pab = bean(PowerAnalBean.class);
    MnetResBean mnrb = bean(MnetResBean.class);
    MummiAnalBean mab = bean(MummiAnalBean.class);
    TandemMSBean tmsb = bean(TandemMSBean.class);

    WorkflowView wfv = bean(WorkflowView.class);
        hb.getJavaHistory().put("NA.dummy.SessionBean1", DataUtils.convertObjToJson(sb));
        hb.getJavaHistory().put("NA.dummy.MsetBean", DataUtils.convertObjToJson(mstb));
        hb.getJavaHistory().put("NA.dummy.UtilsBean", DataUtils.convertObjToJson(utb));
        hb.getJavaHistory().put("NA.dummy.WorkflowBean", DataUtils.convertObjToJson(wb));
        hb.getJavaHistory().put("NA.dummy.DiagramView", DataUtils.convertObjToJson(dv));

        List<String> modules = new ArrayList();
        if (wb.getModuleNames().isEmpty()) {
            modules.add(sb.getAnalType());
        } else {
            modules = wb.getModuleNames();
        }
        // Save state based on data type
        for (String module : modules) {
            // Determine the analysis type
            String analTypePrefix = module.length() >= 4 ? module.substring(0, 4) : module;
            System.out.println("analTypePrefix===" + module);
            System.out.println("naviType===" + sb.getNaviType());

            switch (analTypePrefix) {
                case "raw" -> {

                    hb.getJavaHistory().put("NA.dummy.SpectraParamBean", DataUtils.convertObjToJson(spmb));
                    hb.getJavaHistory().put("NA.dummy.SpectraProcessBean", DataUtils.convertObjToJson(sppb));
                    hb.getJavaHistory().put("NA.dummy.SpectraControlBean", DataUtils.convertObjToJson(spcb));
                }
                case "mset" -> {

                    hb.getJavaHistory().put("NA.dummy.IntegProcessBean", DataUtils.convertObjToJson(itpb));
                    hb.getJavaHistory().put("NA.dummy.IntegResBean", DataUtils.convertObjToJson(itrb));
                }

                case "path" -> {

                    hb.getJavaHistory().put("NA.dummy.PathBean", DataUtils.convertObjToJson(patb));
                    hb.getJavaHistory().put("NA.dummy.IntegResBean", DataUtils.convertObjToJson(itrb));
                }

                default -> {
                    // Handle specific analysis types
                    switch (module) {
                        case "metapaths":

                            hb.getJavaHistory().put("NA.dummy.MetaPathLoadBean", DataUtils.convertObjToJson(mplb));
                            hb.getJavaHistory().put("NA.dummy.MetaPathStatBean", DataUtils.convertObjToJson(mpsb));
                            break;

                        case "metadata":

                            hb.getJavaHistory().put("NA.dummy.MetaLoadBean", DataUtils.convertObjToJson(mlb));
                            break;
                        case "dose":

                            hb.getJavaHistory().put("NA.dummy.DoseResponseBean", DataUtils.convertObjToJson(drb));
                            hb.getJavaHistory().put("NA.dummy.MultifacBean", DataUtils.convertObjToJson(mfb));
                            break;
                        case "roc":

                            hb.getJavaHistory().put("NA.dummy.RocAnalBean", DataUtils.convertObjToJson(rab));
                            break;

                        case "power":

                            hb.getJavaHistory().put("NA.dummy.PowerAnalBean", DataUtils.convertObjToJson(pab));
                            break;

                        case "mf":
                            hb.getJavaHistory().put("NA.dummy.MultifacBean", DataUtils.convertObjToJson(mfb));
                            break;

                        case "network":

                            hb.getJavaHistory().put("NA.dummy.MnetResBean", DataUtils.convertObjToJson(mnrb));
                            break;

                        case "mummichog":
                        case "mass_table":
                        case "mass_all":
                            hb.getJavaHistory().put("NA.dummy.PeakUploadBean", DataUtils.convertObjToJson(pub));
                            hb.getJavaHistory().put("NA.dummy.MummiAnalBean", DataUtils.convertObjToJson(mab));
                            break;

                        case "tandemMS":

                            hb.getJavaHistory().put("NA.dummy.TandemMSBean", DataUtils.convertObjToJson(tmsb));
                            break;

                        default:
                            // Handle other cases if needed
                            break;
                    }
                }
            }
        }
    }
}

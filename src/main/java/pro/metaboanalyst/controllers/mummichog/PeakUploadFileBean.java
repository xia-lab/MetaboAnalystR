/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.mummichog;

import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Named;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author jianguox
 */
@RequestScoped
@Named("peakFileLoader")
public class PeakUploadFileBean implements Serializable {

    @Inject
    ApplicationBean1 ab;
    @Inject
    SessionBean1 sb;
    private static final Logger LOGGER = LogManager.getLogger(PeakUploadBean.class);
    @Inject
    MummiAnalBean mb;

    private UploadedFile peakFile;
    private UploadedFile peakFileTable;

    public UploadedFile getPeakFile() {
        return peakFile;
    }

    public void setPeakFile(UploadedFile peakFile) {
        this.peakFile = peakFile;
    }

    public UploadedFile getPeakFileTable() {
        return peakFileTable;
    }

    public void setPeakFileTable(UploadedFile peakFileTable) {
        this.peakFileTable = peakFileTable;
    }
    
    

}

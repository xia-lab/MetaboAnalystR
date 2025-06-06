/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import java.io.IOException;
import java.io.Serializable;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.StreamedContent;

/**
 *
 * @author Jeff
 */
public class ResultBean implements Serializable {

    // two column result table
    private String fileNameA;
    private String fileNameB;
    private String fileNameALink;
    private String fileNameBLink;

    public String getFileNameA() {
        return fileNameA;
    }

    public String getFileNameB() {
        return fileNameB;
    }

    //get streamed file in getter method to avoid closed stream exception error
    public StreamedContent getFileNameAContent() throws IOException {
        StreamedContent ct = null;
        if (!fileNameALink.equals("")) {
            ct = DataUtils.getDownloadFile(fileNameALink);
        }
        return ct;
    }
    
    public StreamedContent getFileNameBContent() throws IOException {
        StreamedContent ct = null;
        if (!fileNameBLink.equals("")) {
            ct = DataUtils.getDownloadFile(fileNameBLink);
        }
        return ct;
    }



    public ResultBean(String fileNameA, String fileNameB, String fileNameALink, String fileNameBLink) {
        this.fileNameA = fileNameA;
        this.fileNameB = fileNameB;
        this.fileNameALink = fileNameALink;
        this.fileNameBLink = fileNameBLink;
    }

}

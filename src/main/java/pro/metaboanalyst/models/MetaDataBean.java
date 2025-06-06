/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.SessionBean1;

/**
 *
 * @author zgy
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class MetaDataBean implements Serializable {

    private int id;
    private String name;
    private String param;
    private String status;
    private boolean included;
    private boolean primary;
    private String histPath;
    private List<SampleBean> metaEditBeans = null;
    private boolean disabled;
    private SessionBean1 sb;

    public MetaDataBean(SessionBean1 sb, String name, String param, int id, boolean primary, boolean disabled, String status) {
        this.sb = sb;
        this.name = name;
        this.param = param;
        this.id = id;
        this.primary = primary;
        this.disabled = disabled;
        this.status = status;
    }

    public boolean isPrimary() {
        return primary;
    }

    public void setPrimary(boolean primary) {
        this.primary = primary;
    }

    public List<SampleBean> getMetaEditBeans() {
        if (metaEditBeans == null) {
            metaEditBeans = createMetaEditBeans(sb.getRConnection(), name, false);
        }
        return metaEditBeans;
    }

    public void setMetaEditBeans(List<SampleBean> metaEditBeans) {
        this.metaEditBeans = metaEditBeans;
    }

    private List<SampleBean> createMetaEditBeans(RConnection RC, String metaname, boolean withNA) {
        String[] allMetas = RDataUtils.getUniqueMetaNames(RC, metaname);
        int samSize = allMetas.length;
        List<SampleBean> samNABeans = new ArrayList();
        if (withNA) {
            samNABeans.add(new SampleBean("<Not set>", "NA")); // the first one is NA
        }
        for (int i = 0; i < samSize; i++) {
            samNABeans.add(new SampleBean(allMetas[i], allMetas[i]));
        }
        return samNABeans;
    }

    public boolean isDisabled() {
        return disabled;
    }

    public void setDisabled(boolean disabled) {
        this.disabled = disabled;
    }

    public String getHistPath() {
        return histPath;
    }

    public void setHistPath(String histPath) {
        this.histPath = histPath;
    }

    public boolean isIncluded() {
        return included;
    }

    public void setIncluded(boolean included) {
        this.included = included;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getParam() {
        return param;
    }

    public void setParam(String param) {
        this.param = param;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public void updateMetaType() {
        RDataUtils.updateMetaType(sb.getRConnection(), name, param);
        sb.addMessage("info", "Meta type updated!");
    }

}

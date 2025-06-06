/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.beans.ConstructorProperties;

/**
 *
 * @author xia
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RcmdBean {

    private String step;
    private String cmd;

    @JsonCreator
    @ConstructorProperties({"step", "cmd"})
    public RcmdBean (String step, String cmd){
        this.step = step;
        this.cmd = cmd;
    }
    
    public String getStep() {
        return step;
    }

    public void setStep(String step) {
        this.step = step;
    }

    public String getCmd() {
        return cmd;
    }

    public void setCmd(String cmd) {
        this.cmd = cmd;
    }
    
}

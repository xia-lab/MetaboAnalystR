/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.beans.ConstructorProperties;
import java.io.Serializable;

/**
 *
 * @author jianguox
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class NetBean implements Serializable{
    private String name;
    private int queryNum;
    private int nodeNum;
    private int edgeNum;

    @JsonCreator
    @ConstructorProperties({"name", "nodeNum", "edgeNum", "queryNum"})
    public NetBean(String name, int nodeNum, int edgeNum, int queryNum){
        this.name = name;
        this.nodeNum = nodeNum;
        this.edgeNum = edgeNum;
        this.queryNum = queryNum;
    }
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getQueryNum() {
        return queryNum;
    }

    public void setQueryNum(int queryNum) {
        this.queryNum = queryNum;
    }

    public int getNodeNum() {
        return nodeNum;
    }

    public void setNodeNum(int nodeNum) {
        this.nodeNum = nodeNum;
    }

    public int getEdgeNum() {
        return edgeNum;
    }

    public void setEdgeNum(int edgeNum) {
        this.edgeNum = edgeNum;
    }
    
}

/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Named;

/**
 *
 * @author zgy
 */
@SessionScoped
@Named("historyBean")
public class HistoryBean implements Serializable {
    
    private ArrayList<String> beans2Save = new ArrayList<>();
    private LinkedHashMap<String, String> javaHistory = new LinkedHashMap<>();
    private String javaHistoryString;

    public LinkedHashMap<String, String> getJavaHistory() {
        return javaHistory;
    }

    public void setJavaHistory(LinkedHashMap<String, String> javaHistory) {
        this.javaHistory = javaHistory;
    }

    public String getJavaHistoryString() {
        return javaHistoryString;
    }

    public void setJavaHistoryString(String javaHistoryString) {
        this.javaHistoryString = javaHistoryString;
    }

    public ArrayList<String> getBeans2Save() {
        return beans2Save;
    }

    public void setBeans2Save(ArrayList<String> beans2Save) {
        this.beans2Save = beans2Save;
    }
}

/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;
//for mgwasResultModel

import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author zgy
 */
public class CompoundDataModel {

    private String compoundName;
    private List<MRResult> mrResults;

    public CompoundDataModel(String compoundName) {
        this.compoundName = compoundName;
        this.mrResults = new ArrayList<>();
    }

    public void addMrResult(MRResult mrResult) {
        this.mrResults.add(mrResult);
    }

    // Getters and setters
    public String getCompoundName() {
        return compoundName;
    }

    public void setCompoundName(String compoundName) {
        this.compoundName = compoundName;
    }

    public List<MRResult> getMrResults() {
        return mrResults;
    }

    public void setMrResults(List<MRResult> mrResults) {
        this.mrResults = mrResults;
    }
}

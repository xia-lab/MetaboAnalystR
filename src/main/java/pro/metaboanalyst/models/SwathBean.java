/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;

import java.io.Serializable;

/**
 *
 * @author qiang
 */
public class SwathBean implements Serializable {

    private int idx;
    private double lomz;
    private double upmz;

    public SwathBean(int idx, double lomz, double upmz) {
        this.idx = idx;
        this.lomz = lomz;
        this.upmz = upmz;
    }

    public int getIdx() {
        return idx;
    }

    public void setIdx(int idx) {
        this.idx = idx;
    }

    public double getLomz() {
        return lomz;
    }

    public void setLomz(double lomz) {
        this.lomz = lomz;
    }

    public double getUpmz() {
        return upmz;
    }

    public void setUpmz(double upmz) {
        this.upmz = upmz;
    }


}

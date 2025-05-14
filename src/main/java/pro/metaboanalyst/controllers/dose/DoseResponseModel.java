/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.dose;

/**
 *
 * @author soufanom
 */
public class DoseResponseModel {
    
	private double[][] data;
	private double[][] dataMean;
	private double[] dose;
	private String[] dataMeanColNms;
	private String[] items;
	private String[] models;
	private double[][] drcRes;
	private String[] drcResColNms;
	private String[] drcModelNms;
	private String[] drcInvStatus;
	private String[] drcResGeneIds;
        private String cpus;

	public DoseResponseModel() {
	}
	
	public DoseResponseModel(double[][] data, double[][] dataMean, double[] dose, 
			String[] dataMeanColNms, String[] items, String[] models, double[][] drcRes, 
			String[] drcResColNms, String[] drcModelNms, String[] drcInvStatus,
			String[] drcResGeneIds) {
		this.data = data;
		this.dataMean = dataMean;
		this.dose = dose;
		this.dataMeanColNms = dataMeanColNms; 
		this.items = items;
		this.models = models;
		this.drcRes = drcRes;
		this.drcResColNms = drcResColNms;
		this.drcModelNms = drcModelNms;
		this.drcInvStatus = drcInvStatus;
		this.drcResGeneIds = drcResGeneIds;
	}	
	
	public double[][] getData() {
		return data;
	}

	public void setData(double[][] data) {
		this.data = data;
	}

	public double[] getDose() {
		return dose;
	}

	public void setDose(double[] dose) {
		this.dose = dose;
	}

	public double[][] getDataMean() {
		return dataMean;
	}

	public void setDataMean(double[][] dataMean) {
		this.dataMean = dataMean;
	}

	public String[] getDataMeanColNms() {
		return dataMeanColNms;
	}

	public void setDataMeanColNms(String[] dataMeanColNms) {
		this.dataMeanColNms = dataMeanColNms;
	}

	public String[] getItems() {
		return items;
	}

	public void setItems(String[] items) {
		this.items = items;
	}
	
	public String[] getModels() {
		return models;
	}

	public void setModels(String[] models) {
		this.models = models;
	}
	
	public double[][] getDrcRes() {
		return drcRes;
	}

	public void setDrcRes(double[][] drcRes) {
		this.drcRes = drcRes;
	}

	public String[] getDrcResColNms() {
		return drcResColNms;
	}

	public void setDrcResColNms(String[] drcResColNms) {
		this.drcResColNms = drcResColNms;
	}

	public String[] getDrcModelNms() {
		return drcModelNms;
	}

	public void setDrcModelNms(String[] drcModelNms) {
		this.drcModelNms = drcModelNms;
	}

	public String[] getDrcInvStatus() {
		return drcInvStatus;
	}

	public void setDrcInvStatus(String[] drcInvStatus) {
		this.drcInvStatus = drcInvStatus;
	}
	
	public String[] getDrcResGeneIds() {
		return drcResGeneIds;
	}

	public void setDrcResGeneIds(String[] drcResGeneIds) {
		this.drcResGeneIds = drcResGeneIds;
	}

        public String getCpus() {
            return cpus;
        }

        public void setCpus(String cpus) {
            this.cpus = cpus;
        }
        

    
}

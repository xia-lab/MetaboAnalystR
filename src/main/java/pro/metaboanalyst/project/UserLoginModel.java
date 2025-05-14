/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.project;

import java.util.Date;

/**
 *
 * @author soufanom
 */


public class UserLoginModel {
    
    private long id = 0; //userNM, a number 
    private String fname = "NA"; 
    private String lname = "NA"; 
    private String institution = "Earth";
    private String password = "NA";
    private String email = "people@world.com";
    private String salt = "NA";
    private String securedPassword = "NA"; 
    //properties for account reset
    private String resetToken = "NA";
    private Date resetExpiration = new Date();
    private int numOfProjects = 0;
    
    
    public UserLoginModel(long id, String email, String password, int numOfProjects, String resetToken, Date resetExpiration, String fname, String institution){
        this.id = id;
        this.email = email;
        this.password = password;
        this.numOfProjects = numOfProjects;
        this.resetToken = resetToken;
        this.resetExpiration = resetExpiration;
        this.institution = institution;
        this.fname = fname;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }
    
    public String getFname() {
        return fname;
    }

    public void setFname(String fname) {
        this.fname = fname;
    }

    public String getLname() {
        return lname;
    }

    public void setLname(String lname) {
        this.lname = lname;
    }

    public String getInstitution() {
        return institution;
    }

    public void setInstitution(String institution) {
        this.institution = institution;
    }

    public String getResetToken() {
        return resetToken;
    }

    public void setResetToken(String resetToken) {
        this.resetToken = resetToken;
    }

    public Date getResetExpiration() {
        return resetExpiration;
    }

    public void setResetExpiration(Date resetExpiration) {
        this.resetExpiration = resetExpiration;
    }
    
    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getSalt() {
        return salt;
    }

    public void setSalt(String salt) {
        this.salt = salt;
    }

    public String getSecuredPassword() {
        return securedPassword;
    }

    public void setSecuredPassword(String securedPassword) {
        this.securedPassword = securedPassword;
    }
    
    public int getNumOfProjects() {
        return numOfProjects;
    }

    public void setNumOfProjects(int numOfProjects) {
        this.numOfProjects = numOfProjects;
    }
    
}

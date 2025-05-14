/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import com.fasterxml.jackson.annotation.JsonProperty;

public class LoginResponse {

    @JsonProperty("kind")
    private String kind;

    @JsonProperty("localId")
    private String localId;

    @JsonProperty("email")
    private String email;

    @JsonProperty("displayName")
    private String displayName;

    @JsonProperty("idToken")
    private String idToken;

    @JsonProperty("registered")
    private boolean registered;

    @JsonProperty("refreshToken")
    private String refreshToken;

    @JsonProperty("expiresIn")
    private String expiresIn;

    // Getter and setter methods here
    public String getKind() {
        return kind;
    }

    public void setKind(String kind) {
        this.kind = kind;
    }

    public String getLocalId() {
        return localId;
    }

    public void setLocalId(String localId) {
        this.localId = localId;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public String getIdToken() {
        return idToken;
    }

    public void setIdToken(String idToken) {
        this.idToken = idToken;
    }

    public boolean isRegistered() {
        return registered;
    }

    public void setRegistered(boolean registered) {
        this.registered = registered;
    }

    public String getRefreshToken() {
        return refreshToken;
    }

    public void setRefreshToken(String refreshToken) {
        this.refreshToken = refreshToken;
    }

    public String getExpiresIn() {
        return expiresIn;
    }

    public void setExpiresIn(String expiresIn) {
        this.expiresIn = expiresIn;
    }
}
/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;

/**
 *
 * @author zgy
 */
public class GalleryImage {
    private String caption;       // Caption or name of the image
    private String downloadUrl;   // URL for downloading and displaying the image
    private String interactiveUrl; // URL for the interactive result page

    public GalleryImage(String caption, String downloadUrl, String interactiveUrl) {
        this.caption = caption;
        this.downloadUrl = downloadUrl;
        this.interactiveUrl = interactiveUrl;
    }

    public String getCaption() {
        return caption;
    }

    public String getDownloadUrl() {
        return downloadUrl;
    }

    public String getInteractiveUrl() {
        return interactiveUrl;
    }
}

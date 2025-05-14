/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.chat;

/**
 *
 * @author zgy
 */
public class Message {
    
    private String user;
    private String content;

    // Constructor
    public Message(String user, String content) {
        this.user = user;
        this.content = content;
    }

    // Getter and Setter for user
    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    // Getter and Setter for content
    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }
}

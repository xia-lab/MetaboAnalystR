package pro.metaboanalyst.agents;

import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Named("chatSession")
@SessionScoped
public class ChatSession implements Serializable {

    private static final long serialVersionUID = 1L;

    List<ChatMessage> history = new ArrayList<>();

    public List<ChatMessage> getHistory() {
        return history;
    }

    public void setHistory(List<ChatMessage> history) {
        this.history = history;
    }


    public class ChatMessage {

        private final String role; // e.g., "user" or "model"
        private final String content;

        public ChatMessage(String role, String content) {
            this.role = role;
            this.content = content;
        }

        public String getRole() {
            return role;
        }

        public String getContent() {
            return content;
        }
    }
}

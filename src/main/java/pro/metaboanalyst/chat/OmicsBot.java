/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.chat;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.Serializable;
import jakarta.inject.Named;
import io.github.sashirestela.openai.SimpleOpenAI;
import io.github.sashirestela.openai.common.content.ContentPart.ContentPartTextAnnotation;
import io.github.sashirestela.openai.domain.assistant.ThreadMessageDelta;
import io.github.sashirestela.openai.domain.assistant.ThreadMessageRequest;
import io.github.sashirestela.openai.domain.assistant.ThreadMessageRole;
import io.github.sashirestela.openai.domain.assistant.ThreadRequest;
import io.github.sashirestela.openai.domain.assistant.ThreadRunRequest;
import io.github.sashirestela.openai.domain.assistant.events.EventName;
import java.util.ArrayList;
import java.util.List;
import jakarta.enterprise.context.SessionScoped;
import org.apache.commons.lang.StringUtils;
import com.vladsch.flexmark.html.HtmlRenderer;
import com.vladsch.flexmark.parser.Parser;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.SessionBean1;

/**
 *
 * @author jeffxia
 */
@Named("myBot")
@SessionScoped
public class OmicsBot implements Serializable {

    private String code = "metabolomics";
    private String assistantId = "asst_3xU9dbI9U8Ss9EBtMaOBU97i";
    private String welcomeMsg = "I am MetaboAnalyst Assistant using GPT-4o mini. Type your question and click on 'Send' to start. Please be patient ....";

    private String promptMsg = "";

    private SimpleOpenAI openAI;
    private String ProjectAPIkey = "sk-omicsassistant-JSCOME4Fn6ZUnJGiGFd4T3BlbkFJg0djNc9rau5bBDJWPVex";//real
    //private String ProjectAPIkey = "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";//test
    //expressanalyst
    private String expressanalystId = "asst_2pI7PJENiw5CaqqICK3pFipo";
    private String expressanalystId_gpt3 = "asst_n3GOekKDQNQeSxfPrf5d3QL8";

    //metaboanalyst
    private String metaboanalystId = "asst_3xU9dbI9U8Ss9EBtMaOBU97i";
    private String metaboanalystId_gpt3 = "asst_AYkAU6Tth3k7XYiTsImOcB1M";

    //microbiomeanalyst
    private String microbiomeanalysId = "asst_ZYlqCF0KvY3LgcMKocT4c1eZ";
    private String microbiomeanalysId_gpt3 = "asst_kX5c15sq4yBubMd9D0ECnQSe";
    //mirnet
    private String mirnetId = "asst_jtANl7U0sw8poIkQ69NGOUDz";
    private String mirnetId_gpt3 = "asst_RRcfYPtCFi0xQXfJ1v8eOIqH";
    //multi-omics
    private String multiomicsId = "asst_RzwY9mLVlHMXl7TfD7wX5JPw";
    private String multiomicsId_gpt3 = "asst_Alx6GipcrlpOyjOKAf2TFeb0";
    
    @JsonIgnore
    @Inject
    private SessionBean1 sb;
    
    private List<Message> messages;

    public List<Message> getMessages() {
        if (messages == null) {
            messages = new ArrayList<>();
            messages.add(new Message("Assistant", welcomeMsg));
        }
        return messages;
    }

    public void setMessages(List<Message> messages) {
        this.messages = messages;
    }

    public void OmicsBot() {
        messages = new ArrayList<>();
        messages.add(new Message("Assistant", welcomeMsg));
    }

    public void initAssistant() {
        messages = new ArrayList<>();

        if ("na".equals(code)) {
            promptMsg = "";
            welcomeMsg = "Welcome! Please first choose your Omics Assistant to start";
            messages.add(new Message("Assistant", welcomeMsg));
            return;
        }

        cleanResource();

        // Get the appropriate assistant ID
        assistantId = getOppositeAssistantId(code);

        String displayModelName = "";
        if (modelName.equals("GPT-3.5")) {
            displayModelName = "GPT-4o mini";
        } else {
            displayModelName = "GPT-3.5";
        }

        switch (code) {
            case "transcriptomics" ->
                welcomeMsg = "I am ExpressAnalyst Assistant using " + displayModelName + ". Type your question and click on 'Send' to start. Please be patient ....";
            case "metabolomics" ->
                welcomeMsg = "I am MetaboAnalyst Assistant using " + displayModelName + ". Type your question and click on 'Send' to start. Please be patient ....";
            case "microbiomics" ->
                welcomeMsg = "I am MicrobiomeAnalyst Assistant using " + displayModelName + ". Type your question and click on 'Send' to start. Please be patient ....";
            case "microrna" ->
                welcomeMsg = "I am miRNet Assistant using " + displayModelName + ". Type your question and click on 'Send' to start. Please be patient ....";
            default ->
                welcomeMsg = "I am OmicsNet/OmicsAnalyst Assistant using " + displayModelName + ". Type your question and click on 'Send' to start. Please be patient ....";
        }
        messages.add(new Message("Assistant", welcomeMsg));
    }

    public String getPromptMsg() {
        return promptMsg;
    }

    public void setPromptMsg(String promptMsg) {
        this.promptMsg = promptMsg;
    }

    public String getWelcomeMsg() {
        return welcomeMsg;
    }

    public void setWelcomeMsg(String welcomeMsg) {
        this.welcomeMsg = welcomeMsg;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    private String threadId; // this is runtime creation

    public String sendMessage(String myQuestion) {
        if ("na".equals(code)) {
            sb.addMessage("error", "You need to first choose an Omics Assistant to start");
            return null;
        }

        //PrimeFaces.current().executeScript("PF('pbClient').start()");
        if (openAI == null) {
            openAI = SimpleOpenAI.builder().apiKey(ProjectAPIkey).build();
        }

        if (threadId == null) {
            var thread = openAI.threads().create(ThreadRequest.builder().build()).join();
            threadId = thread.getId();
            //System.out.println("Thread was created with id: " + threadId);
        }

        //System.out.println("===========" + myQuestion);
        StringBuilder result = new StringBuilder();
        //System.out.println("myquestion===" + myQuestion);

        openAI.threadMessages()
                .create(threadId, ThreadMessageRequest.builder()
                        .role(ThreadMessageRole.USER)
                        .content(myQuestion)
                        .build())
                .join();
        var runStream = openAI.threadRuns()
                .createStream(threadId, ThreadRunRequest.builder()
                        .assistantId(assistantId)
                        .build())
                .join();

        runStream.forEach(event -> {
            switch (event.getName()) {

                case EventName.THREAD_MESSAGE_DELTA -> {
                    var msgDelta = (ThreadMessageDelta) event.getData();
                    var content = msgDelta.getDelta().getContent().get(0);
                    if (content instanceof ContentPartTextAnnotation textContent) {

                        //System.out.print(textContent.getText().getValue());
                        result.append(textContent.getText().getValue());
                        //result.append("\n");
                    }
                }
                case EventName.THREAD_MESSAGE_COMPLETED ->
                    result.append("        ");
                //System.out.println("Complete!");
                default -> {
                }
            }
        });
        //PrimeFaces.current().executeScript("PF('pbClient').cancel()");
        return result.toString();
    }

    public String callOpenAI(String myMsg, String[] params) {

        if ("na".equals(code)) {
            sb.addMessage("error", "You need to first choose an Omics Assistant to start");
            return null;
        }
        String myQuestion = myMsg + StringUtils.join(params, " ");
        return ">> Answer: " + sendMessage(myQuestion);

    }

    public void cleanResource() {
        if (threadId != null) {
            var deletedThread = openAI.threads().delete(threadId).join();
            System.out.println("Thread was deleted: " + deletedThread.getDeleted());
            //PrimeFaces.current().executeScript("PF('OmicsBotDialog').hide();");
            threadId = null;
        } else {
            System.out.println("ThreadId is null, resource");
        }
    }

    // Updated sendMessage method for JSF integration
    public void sendMsg() {
        if ("na".equals(code)) {
            sb.addMessage("error", "You need to first choose an Omics Assistant to start");
            return;
        }

        if (openAI == null) {
            openAI = SimpleOpenAI.builder().apiKey(ProjectAPIkey).build();
        }

        if (threadId == null) {
            var thread = openAI.threads().create(ThreadRequest.builder().build()).join();
            threadId = thread.getId();
            System.out.println("Thread was created with id: " + threadId);
        }

        // Add the user's message to the chat
        getMessages().add(new Message("User", promptMsg));
        StringBuilder result = new StringBuilder();

        openAI.threadMessages()
                .create(threadId, ThreadMessageRequest.builder()
                        .role(ThreadMessageRole.USER)
                        .content(promptMsg)
                        .build())
                .join();

        var runStream = openAI.threadRuns()
                .createStream(threadId, ThreadRunRequest.builder()
                        .assistantId(assistantId)
                        .build())
                .join();

        runStream.forEach(event -> {
            // Log the JSON for each event
            try {
                ObjectMapper objectMapper = new ObjectMapper();
                String eventJson = objectMapper.writeValueAsString(event);
                System.out.println("Event JSON: " + eventJson);
            } catch (Exception e) {
                e.printStackTrace();
            }

            switch (event.getName()) {

                case EventName.THREAD_MESSAGE_DELTA -> {
                    var msgDelta = (ThreadMessageDelta) event.getData();
                    var content = msgDelta.getDelta().getContent().get(0);
                    if (content instanceof ContentPartTextAnnotation textContent) {
                        result.append(textContent.getText().getValue());
                    }
                }
                case EventName.THREAD_MESSAGE_COMPLETED ->
                    result.append("        ");
                default -> {
                }
            }
        });

        String answer = result.toString();

        String regex = "【\\d+(:\\d+)?+†source】";

        String res = answer.replaceAll(regex, "");
        res = getHtmlContent(res);
        // Add the response from OpenAI to the chat
        messages.add(new Message("Assistant", res));
        //System.out.println("msg====" + res);

        // Clear the prompt message for the next input
        promptMsg = "";
        //return;
    }

    private String modelName = "GPT-3.5";

    public void modifyModel() {
        if (modelName.equals("GPT-3.5")) {
            modelName = "GPT-4o mini";
        } else {
            modelName = "GPT-3.5";
        }
        initAssistant();
    }

    public String getModelName() {
        return modelName;
    }

    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    public String getOppositeAssistantId(String code) {
        switch (code) {
            case "transcriptomics":
                return modelName.equals("GPT-3.5") ? expressanalystId : expressanalystId_gpt3;
            case "metabolomics":
                return modelName.equals("GPT-3.5") ? metaboanalystId : metaboanalystId_gpt3;
            case "microbiomics":
                return modelName.equals("GPT-3.5") ? microbiomeanalysId : microbiomeanalysId_gpt3;
            case "microrna":
                return modelName.equals("GPT-3.5") ? mirnetId : mirnetId_gpt3;
            default:
                return modelName.equals("GPT-3.5") ? multiomicsId : multiomicsId_gpt3;
        }
    }

    public String getHtmlContent(String text) {
        Parser parser = Parser.builder().build();
        HtmlRenderer renderer = HtmlRenderer.builder().build();
        return renderer.render(parser.parse(text)); // Render HTML from markdown
    }

}

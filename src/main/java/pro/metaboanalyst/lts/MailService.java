/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import jakarta.enterprise.context.ApplicationScoped;
import com.sendgrid.*;
import com.sendgrid.helpers.mail.*;
import com.sendgrid.helpers.mail.objects.*;
import java.io.IOException;
import jakarta.annotation.PostConstruct;
import jakarta.inject.Named;

@ApplicationScoped
@Named("mailService")
public class MailService {

    private SendGrid sg;
    private String sendGridFromEmail;

    @PostConstruct
    public void initResources() {
        // Initialize SendGrid instance with API key from environment variable
        //sendGridFromEmail = System.getProperty("sendgrid.fromemail");
        sendGridFromEmail = "support@xialab.ca";
        this.sg = new SendGrid("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");
        //System.getenv("SENDGRID_API_KEY")
        //System.out.println(System.getenv("SENDGRID_API_KEY") + "=====apikey");
        if (sendGridFromEmail == null) {
            System.clearProperty("sendgrid.fromemail");
        } else {
            System.setProperty("sendgrid.fromemail", "support@xialab.ca");
        }
    }

    /**
     * Sends an email using SendGrid
     *
     * @param fromEmail Sender's email address
     * @param toEmail Recipient's email address
     * @param subject Subject of the email
     * @param contentType Content type (e.g., "text/plain", "text/html")
     * @param emailContent Email content
     * @return Response from SendGrid
     * @throws IOException If an error occurs while sending the email
     */
    public boolean sendEmail(String toEmail, String subject, String contentType, String emailContent) throws IOException {
        Email to = new Email(toEmail);
        Email from = new Email(sendGridFromEmail);
        Content content = new Content(contentType, emailContent);
        Mail mail = new Mail(from, subject, to, content);

        Request request = new Request();
        try {
            request.setMethod(Method.POST);
            request.setEndpoint("mail/send");
            request.setBody(mail.build());
            Response response = sg.api(request);

            // Print the response for debugging
            //System.out.println(response.getStatusCode());
            //System.out.println(response.getBody());
            //System.out.println(response.getHeaders());

            // Check response code for success
            if (response.getStatusCode() >= 200 && response.getStatusCode() < 300) {
                return true;
            } else {
                // Log or handle the specific error details as needed
                System.err.println("Error sending email. Response code: " + response.getStatusCode());
                System.err.println("Response body: " + response.getBody());
                return false;
            }
        } catch (IOException ex) {
            System.err.println("Exception while sending email: " + ex.getMessage());
            return false;
        }
    }
}

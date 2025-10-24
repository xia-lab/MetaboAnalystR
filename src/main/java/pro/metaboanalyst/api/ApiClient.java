package pro.metaboanalyst.api;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class ApiClient {
    private static final String BASE_URL = "https://www.xialab.ca/rest";
    private static final String TOKEN = "mySecretKey12345678901234567890123456789012";
    private final HttpClient client;

    public ApiClient() {
        client = HttpClient.newHttpClient();
    }

    private HttpRequest.Builder createRequest(String endpoint) {
        //System.out.println(BASE_URL + endpoint);
        return HttpRequest.newBuilder()
                .uri(URI.create(BASE_URL + endpoint))
                .header("Authorization", "Bearer " + TOKEN)
                .header("Content-Type", "application/json");
    }

    public String get(String endpoint) throws Exception {
        HttpRequest request = createRequest(endpoint)
                .GET()
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        return response.body();
    }

    public String post(String endpoint, String jsonBody) throws Exception {
        HttpRequest request = createRequest(endpoint)
                .POST(HttpRequest.BodyPublishers.ofString(jsonBody))
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        return response.body();
    }

    public String put(String endpoint, String jsonBody) throws Exception {
        HttpRequest request = createRequest(endpoint)
                .PUT(HttpRequest.BodyPublishers.ofString(jsonBody))
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        return response.body();
    }

    public String delete(String endpoint) throws Exception {
        HttpRequest request = createRequest(endpoint)
                .DELETE()
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        return response.body();
    }

    public String delete(String string, String string0) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }
}

/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;

/**
 *
 * @author zgy
 */
public class DiagramUtils {

    public static void saveDiagramState(DiagramState diagramState, String filePath) {
        ObjectMapper objectMapper = new ObjectMapper();

        try {
            // Write the DiagramState object to a JSON file
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(new File(filePath), diagramState);
            System.out.println("Diagram state saved to JSON file: " + filePath);
        } catch (IOException e) {
            System.err.println("Error saving diagram state to JSON file: " + e.getMessage());
        }
    }

    public static DiagramState loadDiagramState(String filePath) {
        ObjectMapper objectMapper = new ObjectMapper();

        try {
            // Read the JSON file into a DiagramState object
            return objectMapper.readValue(new File(filePath), DiagramState.class);
        } catch (IOException e) {
            System.err.println("Error loading diagram state from JSON file: " + e.getMessage());
            return null;
        }
    }
}

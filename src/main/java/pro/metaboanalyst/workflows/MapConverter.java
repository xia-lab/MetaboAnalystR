/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;
import jakarta.faces.convert.FacesConverter;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.HashMap;

@FacesConverter("mapConverter")
public class MapConverter implements Converter<Object> {
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
        try {
            return objectMapper.writeValueAsString(value);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public Object getAsObject(FacesContext context, UIComponent component, String value) {
        try {
            return objectMapper.readValue(value, HashMap.class);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}

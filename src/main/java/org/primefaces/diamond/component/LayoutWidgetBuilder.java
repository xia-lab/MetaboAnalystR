package org.primefaces.diamond.component;

import java.io.IOException;
import jakarta.faces.context.FacesContext;

import jakarta.faces.context.ResponseWriter;
import jakarta.servlet.http.HttpServletRequest;

import org.primefaces.util.WidgetBuilder;

public class LayoutWidgetBuilder extends WidgetBuilder {

    public LayoutWidgetBuilder(FacesContext context) {
        super(context, null);
    }

    public LayoutWidgetBuilder ready(String widgetClass, String widgetVar, String id) throws IOException {
        HttpServletRequest req = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
        String pathname = req.getContextPath() + req.getServletPath();

        ResponseWriter rw = context.getResponseWriter();
        rw.startElement("script", null);
        rw.writeAttribute("id", id + "_s", null);
        rw.writeAttribute("type", "text/javascript", null);
        rw.write("$(function(){");
            rw.write("PrimeFaces.cw(\"");
            rw.write(widgetClass);
            rw.write("\",\"");
            rw.write(widgetVar);
            rw.write("\",{id:\"");
            rw.write(id);
            rw.write("\"");
            //attrs
            rw.write(",");
            rw.write("pathname:\"");
            rw.write(pathname);
            rw.write("\"");
        return this;
    }

    @Override
    public void finish() throws IOException {
        ResponseWriter rw = context.getResponseWriter();
        rw.write("});");

        rw.write("});");
        rw.endElement("script");
    }
}

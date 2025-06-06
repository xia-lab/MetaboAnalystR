/*
   Copyright 2009-2022 PrimeTek.

   Licensed under PrimeFaces Commercial License, Version 1.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   Licensed under PrimeFaces Commercial License, Version 1.0 (the "License");

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */
package org.primefaces.diamond.component;

import jakarta.faces.component.UIComponent;
import org.primefaces.component.menu.AbstractMenu;
import jakarta.faces.context.FacesContext;
import jakarta.faces.component.UINamingContainer;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.event.AbortProcessingException;
import jakarta.faces.event.ComponentSystemEvent;
import jakarta.faces.event.ComponentSystemEventListener;
import jakarta.faces.event.ListenerFor;
import jakarta.faces.event.PostAddToViewEvent;
import org.primefaces.component.api.Widget;

@ListenerFor(sourceClass = DiamondMenu.class, systemEventClass = PostAddToViewEvent.class)
public class DiamondMenu extends AbstractMenu implements Widget,ComponentSystemEventListener {

    public static final String COMPONENT_TYPE = "org.primefaces.component.DiamondMenu";
    public static final String COMPONENT_FAMILY = "org.primefaces.component";
    private static final String DEFAULT_RENDERER = "org.primefaces.component.diamondMenuRenderer";
    private static final String[] LEGACY_RESOURCES = new String[]{"primefaces.css","jquery/jquery.js","jquery/jquery-plugins.js","primefaces.js"};
    private static final String[] MODERN_RESOURCES = new String[]{"components.css","jquery/jquery.js","jquery/jquery-plugins.js","core.js"};
    
    protected enum PropertyKeys {

        widgetVar, model, style, styleClass, statefulScroll;

        String toString;

        PropertyKeys(String toString) {
            this.toString = toString;
        }

        PropertyKeys() {
        }

        public String toString() {
            return ((this.toString != null) ? this.toString : super.toString());
        }
    }

    public DiamondMenu() {
        setRendererType(DEFAULT_RENDERER);
    }

    public String getFamily() {
        return COMPONENT_FAMILY;
    }

    public java.lang.String getWidgetVar() {
        return (java.lang.String) getStateHelper().eval(PropertyKeys.widgetVar, null);
    }

    public void setWidgetVar(java.lang.String _widgetVar) {
        getStateHelper().put(PropertyKeys.widgetVar, _widgetVar);
    }

    public org.primefaces.model.menu.MenuModel getModel() {
        return (org.primefaces.model.menu.MenuModel) getStateHelper().eval(PropertyKeys.model, null);
    }

    public void setModel(org.primefaces.model.menu.MenuModel _model) {
        getStateHelper().put(PropertyKeys.model, _model);
    }

    public java.lang.String getStyle() {
        return (java.lang.String) getStateHelper().eval(PropertyKeys.style, null);
    }

    public void setStyle(java.lang.String _style) {
        getStateHelper().put(PropertyKeys.style, _style);
    }

    public java.lang.String getStyleClass() {
        return (java.lang.String) getStateHelper().eval(PropertyKeys.styleClass, null);
    }

    public void setStyleClass(java.lang.String _styleClass) {
        getStateHelper().put(PropertyKeys.styleClass, _styleClass);
    }
    
    public java.lang.Boolean isStatefulScroll() {
        return (java.lang.Boolean) getStateHelper().eval(PropertyKeys.statefulScroll, true);
    }

    public void setStatefulScroll(boolean _statefulScroll) {
        getStateHelper().put(PropertyKeys.statefulScroll, _statefulScroll);
    }

    public String resolveWidgetVar() {
        FacesContext context = getFacesContext();
        String userWidgetVar = (String) getAttributes().get("widgetVar");

        if (userWidgetVar != null) {
            return userWidgetVar;
        } else {
            return "widget_" + getClientId(context).replaceAll("-|" + UINamingContainer.getSeparatorChar(context), "_");
        }
    }
    
    @Override
    public void processEvent(ComponentSystemEvent event) throws AbortProcessingException {
        if(event instanceof PostAddToViewEvent) {
            FacesContext context = getFacesContext();
            UIViewRoot root = context.getViewRoot();
            
            boolean isPrimeConfig;
            try {
                isPrimeConfig = Class.forName("org.primefaces.config.PrimeConfiguration") != null;
            } catch (ClassNotFoundException e) {
                isPrimeConfig = false;
            }

            String[] resources = (isPrimeConfig) ? MODERN_RESOURCES : LEGACY_RESOURCES;

            for(String res : resources) {
                UIComponent component = context.getApplication().createComponent(UIOutput.COMPONENT_TYPE);
                if(res.endsWith("css"))
                    component.setRendererType("jakarta.faces.resource.Stylesheet");
                else if(res.endsWith("js"))
                    component.setRendererType("jakarta.faces.resource.Script");

                component.getAttributes().put("library", "primefaces");
                component.getAttributes().put("name", res);

                root.addComponentResource(context, component);
            }
        }
    }
}
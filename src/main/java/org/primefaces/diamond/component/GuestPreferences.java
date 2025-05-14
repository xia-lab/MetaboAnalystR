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

import java.io.Serializable;
import jakarta.inject.Named;
import org.primefaces.PrimeFaces;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.SessionBean1;

@Named
@SessionScoped
public class GuestPreferences implements Serializable {

    @Inject
    private SessionBean1 sb;
    private String layout = "slim";
    private String scheme = "dim";
    private String componentTheme = "blue";
    private String menuTheme = "darkgray";
    private String logoColor = "white";
    private String inputStyle = "outlined";

    public String getLayout() {
        if (sb.isOverviewPage()) {
            return "overlay";
        }
        return this.layout;
    }

    public void setLayout(String layout) {
        this.layout = layout;
    }

    public String getComponentTheme() {
        return this.componentTheme;
    }

    public void setComponentTheme(String componentTheme) {
        this.componentTheme = componentTheme;
    }

    public String getMenuTheme() {
        if (this.scheme.equals("light")) {
            return menuTheme;
        } else {
            return this.scheme;
        }
    }

    public void setMenuTheme(String menuTheme) {
        this.menuTheme = menuTheme;
    }

    public String getLayoutClass() {
        return "layout-" + getLayout();
    }

    public String getSidebarThemeClass() {
        if (this.scheme.equals("light")) {
            return "layout-sidebar-" + this.menuTheme;
        } else {
            return "layout-sidebar-" + this.scheme;
        }
    }

    public String getInputStyleClass() {
        return this.inputStyle.equals("filled") ? "ui-input-filled" : "";
    }

    public String getLogoColor() {
        return logoColor;
    }

    public void setLogoColor(String logoColor) {
        this.logoColor = logoColor;
    }

    public String getInputStyle() {
        return inputStyle;
    }

    public void setInputStyle(String inputStyle) {
        this.inputStyle = inputStyle;
    }

    public String getScheme() {
        return this.scheme;
    }

    public void setScheme(String scheme) {
        this.scheme = scheme;
    }

    public void onLayoutChange() {
        PrimeFaces.current().executeScript("PrimeFaces.DiamondConfigurator.changeLayout('" + layout + "')");
    }

    public void onSchemeChange() {

        PrimeFaces.current().executeScript("PrimeFaces.DiamondConfigurator.changeMenuTheme('" + scheme + "', 'white')");
        PrimeFaces.current().executeScript("PrimeFaces.DiamondConfigurator.changeScheme('" + scheme + "')");
        PrimeFaces.current().executeScript("updateIframeTheme()");
        sb.addMessage("info", "The current theme has been switched to " + scheme + " mode.");
    }
}

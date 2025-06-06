/** 
 * PrimeFaces Diamond Layout
 */
PrimeFaces.widget.Diamond = PrimeFaces.widget.BaseWidget.extend({

    init: function (cfg) {
        this._super(cfg);
        this.wrapper = $(document.body).children('.layout-wrapper');
        this.contentWrapper = this.wrapper.children('.layout-content-wrapper');
        this.topbar = this.contentWrapper.find('.layout-topbar');
        this.menuButton = this.topbar.find('> .topbar-left > .menu-button');
        this.sidebar = this.wrapper.children('.layout-sidebar');
        this.sidebarRight = this.wrapper.children('.layout-sidebar-right');
        this.menu = this.jq;
        this.menuContainer = this.sidebar.children('.layout-menu-container');
        this.menulinks = this.menu.find('a');
        this.layoutSearch = this.wrapper.children('.layout-search');
        this.layoutSearchContainer = this.layoutSearch.children('.search-container');
        this.layoutSearchInput = this.layoutSearchContainer.find('input:first');

        this.topbarMenu = this.topbar.find('.topbar-right > .topbar-menu');
        this.topbarItems = this.topbarMenu.children('li');
        this.topbarLinks = this.topbarItems.children('a');

        this.configButton = $('#layout-config-button');
        this.configurator = this.wrapper.children('.layout-config');

        this.mask = this.wrapper.children('.layout-mask');
   
        this.restoreMenuState();
        
        this._bindEvents();

        this.topbarMenuClick = false;
        this.menuClick = false;
        this.searchClick = false;
        this.sidebarRightClick = false;
        this.menuActive = false;
        this.configClicked = false;
    },
    
    _bindEvents: function () {
        var $this = this;

        this.sidebar.off('click.menu').on('click.menu', function () {
            $this.menuClick = true;
        });
        
        this.sidebarRight.off('click.menu').on('click.menu', function () {
            $this.sidebarRightClick = true;
        });

        this.menuButton.off('click.menu').on('click.menu', function (e) {
            $this.menuClick = true;

            if ($this.isMobile()) {
                $this.wrapper.toggleClass('layout-mobile-active');
                $(document.body).toggleClass('blocked-scroll');
            }
            else {
                if ($this.isStaticMenu()) {
                    $this.wrapper.toggleClass('layout-static-inactive');
                    $this.saveStaticMenuState();
                }
                else {
                    $this.wrapper.toggleClass('layout-overlay-active');
                }
            }

            e.preventDefault();
        });

        this.layoutSearchContainer.off('click').on('click', function(e) {
            $this.searchClick = true;
        });

        this.layoutSearchInput.off('keydown.search').on('keydown.search', function() {
            var key = event.which;
            
            //escape, tab and enter
            if (key === 27 || key === 9 || key === 13) {
                $this.hideSearch();
            }
        });

        this.menu.find('> li').off('mouseenter.menu').on('mouseenter.menu', function(e) {    
            if (($this.isSlimMenu()) && $this.menuActive) {
                var item = $(this),
                submenu = item.children('ul');
                
                if (!item.hasClass('active-menuitem')) {
                    $this.menu.find('.active-menuitem').removeClass('active-menuitem');
                    item.find('.fade-in-right').removeClass('fade-in-right');
                    item.addClass('active-menuitem');
                }

                $this.calculatePosition(submenu.get(0), item.get(0));
            }
        });

        this.menulinks.off('click.menu').on('click.menu', function (e) {
            var link = $(this),
            item = link.parent('li'),
            submenu = item.children('ul');
            
            if ($this.isSlimMenu()) {
                submenu.css('display','');

                if (item.hasClass('active-menuitem')) {
                    if (submenu.length) {
                        e.preventDefault();
                    }
    
                    if (item.parent().is($this.jq)) {
                        $this.menuActive = false;
                        submenu.addClass('fade-out-right');

                        setTimeout(function () {
                            item.removeClass('active-menuitem');
                            submenu.removeClass('fade-out-right').addClass('fade-in-right');
                        }, 350);
                    }
                    else {
                        item.removeClass('active-menuitem');
                        e.preventDefault();
                    }
                }
                else {
                    if (submenu.length) {
                        e.preventDefault();
                    }

                    if (item.parent().is($this.jq)) {
                        $this.menuActive = true;
                        submenu.addClass('fade-in-right');
                        item.addClass('active-menuitem');
                    }
                    else {
                        item.siblings('.active-menuitem').removeClass('active-menuitem');
                        item.addClass('active-menuitem');
                    }
                }
            }
            else {
                if (submenu.length) {
                    if (item.hasClass('active-menuitem')) {
                        submenu.slideUp(400, function() {
                            item.removeClass('active-menuitem');
                        });
                    }
                    else {
                        $this.deactivateItems(item.siblings());
                        $this.activate(item);
                    }
    
                    e.preventDefault();
                }
                else {
                    link.addClass('active-route');
                    $this.menu.find('.active-route').removeClass('active-route');
                    localStorage.setItem('diamond_menu_scroll_state', link.attr('href') + ',' + $this.menuContainer.scrollTop());
                }
            }

            $this.calculatePosition(submenu.get(0), item.get(0));
        });

        this.topbarLinks.off('click.topbar').on('click.topbar', function (e) {
            var link = $(this),
            item = link.parent(),
            submenu = link.next();
            $this.topbarMenuClick = true;

            item.siblings('.active-menuitem').removeClass('active-menuitem');

            if (submenu.length) {
                if (item.hasClass('active-menuitem')) {
                    $this.hideTopBarSubMenu(item);
                }
                else {
                    item.addClass('active-menuitem');
                    submenu.addClass('fade-in-up');
                }
            }

            if (item.hasClass('search-item')) {
                $this.searchClick = true;
                $this.layoutSearch.addClass('layout-search-active');
                $this.layoutSearchInput.trigger('focus');
            }
            else if (item.hasClass('right-sidebar-item')) {
                $this.sidebarRightClick = true;
                $this.sidebarRight.addClass('layout-sidebar-right-active');
            }
        });
        
        this.configButton.off('click.configbutton').on('click.configbutton', function(e) {
            $this.configurator.toggleClass('layout-config-active');
            $this.configClicked = true;
        });
        
        this.configurator.off('click.config').on('click.config', function() {
            $this.configClicked = true;
        });

        $(document.body).off('click.layout').on('click.layout', function () {
            if (!$this.menuClick) {
                $this.wrapper.removeClass('layout-overlay-active layout-mobile-active');
                $(document.body).removeClass('blocked-scroll');

                if ($this.isSlimMenu()) {
                    var activeRootItem = $this.menu.children('.active-menuitem'),
                    submenu = activeRootItem.children('ul');
                    submenu.addClass('fade-out-right');

                    setTimeout(function () {
                        activeRootItem.removeClass('active-menuitem');
                        submenu.removeClass('fade-out-right').addClass('fade-in-right');
                        $this.menu.find('.active-menuitem').removeClass('active-menuitem');
                    }, 350);

                    $this.menuActive = false;
                } 
            }

            if (!$this.topbarMenuClick) {
                $this.hideTopBarSubMenu($this.topbarItems.filter('.active-menuitem'));
            }

            if (!$this.searchClick && $this.layoutSearch.hasClass('layout-search-active')) {
                $this.hideSearch();
            }

            if (!$this.sidebarRightClick && $this.sidebarRight.hasClass('layout-sidebar-right-active')) {
                $this.sidebarRight.removeClass('layout-sidebar-right-active');
            }
            
            if (!$this.configClicked && $this.configurator.hasClass('layout-config-active')) {
                $this.configurator.removeClass('layout-config-active');
            }

            $this.menuClick = false;
            $this.topbarMenuClick = false;
            $this.searchClick = false;
            $this.sidebarRightClick = false;
            $this.configClicked = false;
        });
    },

    calculatePosition: function(overlay, target) {
        if (overlay) {
            // reset
            overlay.style.top = overlay.style.left = '';

            var rect = target.getBoundingClientRect();
            var left = rect.left, top = rect.top;
            var vWidth = window.innerWidth, vHeight = window.innerHeight;
            var oWidth = overlay.offsetWidth, oHeight = overlay.offsetHeight;
            var scrollbarWidth = this.calculateScrollbarWidth();

            if (this.isHorizontalMenu()) {
                var width = left + oWidth + scrollbarWidth;
                overlay.style.left = (vWidth < width ? (left - (width - vWidth)) : left) + 'px';
            } else if (this.isSlimMenu()) {
                var height = top + oHeight;
                overlay.style.top = (vHeight < height ? (top - (height - vHeight)) : top) + 'px';
            }
        }
    },

    calculateScrollbarWidth: function() {
        var inner = document.createElement('p');
        inner.style.width = "100%";
        inner.style.height = "200px";
      
        var outer = document.createElement('div');
        outer.style.position = "absolute";
        outer.style.top = "0px";
        outer.style.left = "0px";
        outer.style.visibility = "hidden";
        outer.style.width = "200px";
        outer.style.height = "150px";
        outer.style.overflow = "hidden";
        outer.appendChild (inner);
      
        document.body.appendChild(outer);
        var w1 = inner.offsetWidth;
        outer.style.overflow = 'scroll';
        var w2 = inner.offsetWidth;
        if (w1 == w2) w2 = outer.clientWidth;
      
        document.body.removeChild(outer);
      
        return (w1 - w2);
    },

    hideSearch: function () {
        var $this = this;
        this.layoutSearch.children('.search-container').addClass('fade-out-down');

        setTimeout(function () {
            $this.layoutSearch.removeClass('layout-search-active').children('.search-container').removeClass('fade-out-down');
        }, 350);
    },

    hideTopBarSubMenu: function(item) {
        var submenu = item.children('ul');
        submenu.addClass('fade-out-down');

        setTimeout(function () {
            item.removeClass('active-menuitem');
            submenu.removeClass('fade-out-down');
        }, 350);
    },
    
    toggleClass: function(el, className) {
        if (el.hasClass(className)) {
            el.removeClass(className);
        }
        else {
            el.addClass(className);
        }
    },

    activate: function (item) {
        var submenu = item.children('ul');
        item.addClass('active-menuitem');

        if (submenu.length && !this.isHorizontalMenu() && !this.isSlimMenu()) {
            submenu.slideDown();  
        }
    },

    deactivate: function (item) {
        var submenu = item.children('ul');
        item.removeClass('active-menuitem');

        if (submenu.length && !this.isHorizontalMenu() && !this.isSlimMenu()) {
            submenu.hide();  
        }
    },

    deactivateItems: function (items) {
        var $this = this;

        for (var i = 0; i < items.length; i++) {
            var item = items.eq(i),
                submenu = item.children('ul');

            if (submenu.length) {
                if (item.hasClass('active-menuitem')) {
                    item.removeClass('active-menuitem');

                    submenu.slideUp('normal', function () {
                        $(this).parent().find('.active-menuitem').each(function () {
                            $this.deactivate($(this));
                        });
                    });
                }
                else {
                    item.find('.active-menuitem').each(function () {
                        var subItem = $(this);
                        $this.deactivate(subItem);
                    });
                }
            }
            else if (item.hasClass('active-menuitem')) {
                $this.deactivate(item);
            }
        }
    },
        
    clearActiveItems: function() {
        var activeItems = this.jq.find('li.active-menuitem'),
        subContainers = activeItems.children('ul');

        activeItems.removeClass('active-menuitem');
        if(subContainers && subContainers.length) {
            subContainers.hide();
        }
    },

    clearLayoutState: function() {
        this.clearMenuState();
        this.clearActiveItems();
    },

    clearMenuState: function() {
        localStorage.removeItem('diamond_static_menu_inactive');
    },

    saveStaticMenuState: function() {
        if (this.wrapper.hasClass('layout-static-inactive'))
            localStorage.setItem('diamond_static_menu_inactive', 'diamond_static_menu_inactive');
        else
            localStorage.removeItem('diamond_static_menu_inactive');
    },

    isMobile: function () {
        return window.innerWidth <= 991;
    },

    isStaticMenu: function () {
        return this.wrapper.hasClass('layout-static') && this.isDesktop();
    },

    isHorizontalMenu: function() {
        return this.wrapper.hasClass('layout-horizontal') && this.isDesktop();
    },

    isSlimMenu: function() {
        return this.wrapper.hasClass('layout-slim') && this.isDesktop();
    },

    isDesktop: function () {
        return window.innerWidth > 991;
    },

    restoreMenuState: function () {
        var isSlimMenu = this.wrapper.hasClass('layout-slim');
        var $this = this;

        if (!isSlimMenu && this.isDesktop()) {
            var link = $this.menu.find('a[href^="' + this.cfg.pathname + '"]');
            if (link.length) {               
                link.addClass('active-route');

                var menuitem = link.parents('li:not(.layout-root-menuitem)');
                menuitem.addClass('active-menuitem').children('ul').show();

                setTimeout(function() {
                    $this.restoreScrollState(menuitem);
                }, 100)
            }

            var staticMenuState = localStorage.getItem('diamond_static_menu_inactive');
            if (staticMenuState) {
                this.wrapper.addClass('layout-static-inactive layout-static-inactive-restore');
            }
        }
    },

    restoreScrollState: function(menuitem) {
        var scrollState = localStorage.getItem('diamond_menu_scroll_state');
        if (scrollState) {
            var state = scrollState.split(',');
            if (state[0].startsWith(this.cfg.pathname) || this.isScrolledIntoView(menuitem, state[1])) {
                this.menuContainer.scrollTop(parseInt(state[1], 10));
            }
            else {
                this.scrollIntoView(menuitem.get(0));
                localStorage.removeItem('diamond_menu_scroll_state');
            }
        }
        else if (!this.isScrolledIntoView(menuitem, menuitem.scrollTop())){
            this.scrollIntoView(menuitem.get(0));
        }
    },

    scrollIntoView: function(elem) {
        if (document.documentElement.scrollIntoView) {
            elem.scrollIntoView({ block: "nearest", inline: 'start' });

            var container = $('.layout-menu-container');
            var scrollTop = container.scrollTop();
            if (scrollTop > 0) {
                container.scrollTop(scrollTop + parseFloat(this.topbar.height()));
            }
        }
    },

    isScrolledIntoView: function(elem, scrollTop) {
        var viewBottom = parseInt(scrollTop, 10) + this.menuContainer.height();

        var elemTop = elem.position().top;
        var elemBottom = elemTop + elem.height();

        return ((elemBottom <= viewBottom) && (elemTop >= scrollTop));
    }
});

PrimeFaces.DiamondConfigurator = {

    changeLayout: function(layoutTheme) {
        var wrapper = $(document.body).children('.layout-wrapper');
        switch (layoutTheme) {
            case 'static':
                wrapper.addClass('layout-static').removeClass('layout-overlay layout-slim');
            break;

            case 'overlay':
                wrapper.addClass('layout-overlay').removeClass('layout-static layout-slim');
            break;

            case 'slim':
                wrapper.addClass('layout-slim').removeClass('layout-static layout-overlay');
            break;

            default:
                wrapper.addClass('layout-static').removeClass('layout-overlay layout-slim');
            break;
        }
    },

    changeMenuTheme: function(menuTheme, logoColor, componentTheme) {
        var wrapper = $(document.body).children('.layout-wrapper');
        wrapper.attr('class', wrapper.attr('class').replace(wrapper.attr('data-theme'), menuTheme));
        wrapper.attr('data-theme', menuTheme);

        var sidebarLogo = $('#app-logo'),
        logoPath = sidebarLogo.attr('src');
        
        if (logoPath.indexOf(logoColor) === -1) {
            if (logoColor === 'dark')
                sidebarLogo.attr('src', logoPath.replace('white', 'dark'));
            else
                sidebarLogo.attr('src', logoPath.replace('dark', 'white'));
        }

        if (componentTheme) {
            this.changeComponentTheme(componentTheme);
        }
    },

    changeComponentTheme: function(theme) {
        var library = 'primefaces-diamond';
        var linkElement = $('link[href*="theme.css"]');
        var href = linkElement.attr('href');
        var index = href.indexOf(library) + 1;
        var currentTheme = href.substring(index + library.length);
        this.replaceLink(linkElement, href.replace(currentTheme.split('-')[0], theme));
    },

    changeScheme: function(scheme) {
        var library = 'primefaces-diamond';
        var linkElement = $('link[href*="theme.css"]');
        var href = linkElement.attr('href').split('&')[0];
        var index = href.indexOf(library) + 1;
        var currentTheme = href.substring(index + library.length);
        var themeParts = currentTheme.split('-');
        var themeScheme = themeParts[1];
        this.replaceLink(linkElement, href.replace(new RegExp(themeScheme + '$'), scheme));

        var layoutLinkElement = $('link[href*="layout-"]');
        this.replaceLink(layoutLinkElement, layoutLinkElement.attr('href').replace(themeScheme, scheme));

        this.changeLogos(scheme);
    },

    changeLogos: function(scheme) {
        var logo = $('#footer-logo,#logo-mobile,#invoice-logo');
        
        if (scheme === 'light')
            logo.attr('src', logo.attr('src').replace('white', 'dark'));
        else
            logo.attr('src', logo.attr('src').replace('dark', 'white'));
    },

    beforeResourceChange: function() {
        PrimeFaces.ajax.RESOURCE = null;    //prevent resource append
    },
    
    replaceLink: function(linkElement, href) {
        PrimeFaces.ajax.RESOURCE = 'javax.faces.Resource';

        var isIE = this.isIE();

        if (isIE) {
            linkElement.attr('href', href);
        }
        else {
            var cloneLinkElement = linkElement.clone(false);

            cloneLinkElement.attr('href', href);
            linkElement.after(cloneLinkElement);
            
            cloneLinkElement.off('load').on('load', function() {
                linkElement.remove();
            });
        }
    },

    changeMenuToStatic: function() {
        $('.layout-wrapper').removeClass('layout-overlay layout-horizontal layout-slim').addClass('layout-static');
    },

    changeMenuToOverlay: function() {
        $('.layout-wrapper').removeClass('layout-horizontal layout-static layout-slim').addClass('layout-overlay');
    },

    changeMenuToHorizontal: function() {
        $('.layout-wrapper').removeClass('layout-overlay layout-slim layout-static').addClass('layout-horizontal');
    },

    changeMenuToSlim: function() {
        $('.layout-wrapper').removeClass('layout-overlay layout-static layout-horizontal').addClass('layout-slim');
    },

    changeMenuToDark: function() {
        $('.layout-wrapper').removeClass('layout-menu-light').addClass('layout-menu-dark');
    },

    changeMenuToLight: function() {
        $('.layout-wrapper').removeClass('layout-menu-dark').addClass('layout-menu-light');
    },
    
    isIE: function() {
        return /(MSIE|Trident\/|Edge\/)/i.test(navigator.userAgent);
    },

    updateInputStyle: function(value) {
        if (value === 'filled')
            $(document.body).addClass('ui-input-filled');
        else
            $(document.body).removeClass('ui-input-filled');
    }
};

if (PrimeFaces.widget.InputSwitch) {
    PrimeFaces.widget.InputSwitch = PrimeFaces.widget.InputSwitch.extend({

        init: function (cfg) {
            this._super(cfg);

            if (this.input.prop('checked')) {
                this.jq.addClass('ui-inputswitch-checked');
            }
        },

        check: function () {
            var $this = this;

            this.input.prop('checked', true).trigger('change');
            setTimeout(function () {
                $this.jq.addClass('ui-inputswitch-checked');
            }, 100);
        },

        uncheck: function () {
            var $this = this;

            this.input.prop('checked', false).trigger('change');
            setTimeout(function () {
                $this.jq.removeClass('ui-inputswitch-checked');
            }, 100);
        }
    });
}

if (PrimeFaces.widget.AccordionPanel) {
    PrimeFaces.widget.AccordionPanel = PrimeFaces.widget.AccordionPanel.extend({

        init: function (cfg) {
            this._super(cfg);
            
            this.headers.last().addClass('ui-accordion-header-last');
        }
    });
}

/* Issue #924 is fixed for 5.3+ and 6.0. (compatibility with 5.3) */
if(window['PrimeFaces'] && window['PrimeFaces'].widget.Dialog) {
    PrimeFaces.widget.Dialog = PrimeFaces.widget.Dialog.extend({

        enableModality: function() {
            this._super();
            $(document.body).children(this.jqId + '_modal').addClass('ui-dialog-mask');
        },

        syncWindowResize: function() {}
    });
}

if (PrimeFaces.widget.SelectOneMenu) {
    PrimeFaces.widget.SelectOneMenu = PrimeFaces.widget.SelectOneMenu.extend({
        init: function (cfg) {
            this._super(cfg);

            var $this = this;
            if (this.jq.parent().hasClass('ui-float-label')) {
                this.m_panel = $(this.jqId + '_panel');
                this.m_focusInput = $(this.jqId + '_focus');

                this.m_panel.addClass('ui-input-overlay-panel');
                this.jq.addClass('ui-inputwrapper');

                if (this.input.val() != '') {
                    this.jq.addClass('ui-inputwrapper-filled');
                }

                this.input.off('change').on('change', function () {
                    $this.inputValueControl($(this));
                });

                this.m_focusInput.on('focus.ui-selectonemenu', function () {
                    $this.jq.addClass('ui-inputwrapper-focus');
                })
                    .on('blur.ui-selectonemenu', function () {
                        $this.jq.removeClass('ui-inputwrapper-focus');
                    });

                if (this.cfg.editable) {
                    this.label.on('input', function (e) {
                        $this.inputValueControl($(this));
                    }).on('focus', function () {
                        $this.jq.addClass('ui-inputwrapper-focus');
                    }).on('blur', function () {
                        $this.jq.removeClass('ui-inputwrapper-focus');
                        $this.inputValueControl($(this));
                    });
                }
            }
        },

        inputValueControl: function (input) {
            if (input.val() != '')
                this.jq.addClass('ui-inputwrapper-filled');
            else
                this.jq.removeClass('ui-inputwrapper-filled');
        }
    });
}

if (PrimeFaces.widget.Chips) {
    PrimeFaces.widget.Chips = PrimeFaces.widget.Chips.extend({
        init: function (cfg) {
            this._super(cfg);

            var $this = this;
            if (this.jq.parent().hasClass('ui-float-label')) {
                this.jq.addClass('ui-inputwrapper');

                if ($this.jq.find('.ui-chips-token').length !== 0) {
                    this.jq.addClass('ui-inputwrapper-filled');
                }

                this.input.on('focus.ui-chips', function () {
                    $this.jq.addClass('ui-inputwrapper-focus');
                }).on('input.ui-chips', function () {
                    $this.inputValueControl();
                }).on('blur.ui-chips', function () {
                    $this.jq.removeClass('ui-inputwrapper-focus');
                    $this.inputValueControl();
                });

            }
        },

        inputValueControl: function () {
            if (this.jq.find('.ui-chips-token').length !== 0 || this.input.val() != '')
                this.jq.addClass('ui-inputwrapper-filled');
            else
                this.jq.removeClass('ui-inputwrapper-filled');
        }
    });
}

if (PrimeFaces.widget.DatePicker) {
    PrimeFaces.widget.DatePicker = PrimeFaces.widget.DatePicker.extend({
        init: function (cfg) {
            this._super(cfg);

            var $this = this;
            if (this.jq.parent().hasClass('ui-float-label') && !this.cfg.inline) {
                if (this.input.val() != '') {
                    this.jq.addClass('ui-inputwrapper-filled');
                }

                this.jqEl.off('focus.ui-datepicker blur.ui-datepicker change.ui-datepicker')
                    .on('focus.ui-datepicker', function () {
                        $this.jq.addClass('ui-inputwrapper-focus');
                    })
                    .on('blur.ui-datepicker', function () {
                        $this.jq.removeClass('ui-inputwrapper-focus');
                    })
                    .on('change.ui-datepicker', function () {
                        $this.inputValueControl($(this));
                    });
            }
        },

        inputValueControl: function (input) {
            if (input.val() != '')
                this.jq.addClass('ui-inputwrapper-filled');
            else
                this.jq.removeClass('ui-inputwrapper-filled');
        }
    });
}
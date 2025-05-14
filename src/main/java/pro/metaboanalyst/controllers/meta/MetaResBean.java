/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.meta;

import pro.metaboanalyst.models.ColumnModel;
import pro.metaboanalyst.models.MetaResultBean;
import pro.metaboanalyst.rwrappers.RMetaUtils;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import jakarta.inject.Named;
import jakarta.faces.view.ViewScoped;
import jakarta.faces.model.SelectItem;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.inject.Inject;
import java.util.HashMap;
import java.util.Map;
import org.primefaces.event.TabChangeEvent;
import pro.metaboanalyst.rwrappers.RDataUtils;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("metaResBean")
public class MetaResBean implements Serializable {

    @Inject
    SessionBean1 sb;

    private List<MetaResultBean> resBeans;
    private List<ColumnModel> columns = new ArrayList();
    private Map<String, List<MetaResultBean>> resBeanMap = new HashMap();
    private String selectedKey;

    public MetaResBean() {

        resBeans = new ArrayList();
        populateResBeans();

    }

    private String[] ids = null;

    public void populateResBeans() {
        SessionBean1 sb1 = (SessionBean1) DataUtils.findBean("sessionBean1");
        MetaLoadBean mb = (MetaLoadBean) DataUtils.findBean("loadBean");

        RConnection RC = sb1.getRConnection();
        if (RMetaUtils.checkMetaPerformed(RC, mb.getAnalMethod()) == 0) {
            return;
        } else {
            RDataUtils.toggleMetaRes(RC, mb.getAnalMethod());
        }

        resBeans.clear();
        columns.clear();

        String[] columnKeys = RMetaUtils.getMetaResColNames(RC);
        ids = RMetaUtils.getMetaResGeneIDs(RC);
        double[][] resMat = RMetaUtils.getMetaResMatrix(RC, indField);
        columns.add(new ColumnModel("ID", "ID", "string")); //add entrez ID column
        for (String columnKey : columnKeys) {
            columns.add(new ColumnModel(columnKey, columnKey, "double"));
        }

        tableFields = new SelectItem[columns.size()];
        for (int k = 0; k < tableFields.length; k++) {
            ColumnModel cm = columns.get(k);
            tableFields[k] = new SelectItem(cm.getHeader(), cm.getHeader() + "  ");
        }
        sortField = tableFields[tableFields.length - 1].getValue().toString();
        System.out.println(sortField + "===sortField");
        //set up object list
        // due to memory/performance issue, cannot display more than 5000 sig genes
        // otherwise java.lang.ArrayIndexOutOfBoundsException: 5000
        if (ids != null && ids.length > 0) {
            for (int i = 0; i < ids.length; i++) {
                //we use gene symbol as ID for sorting purpose as actual 
                //gene symbol is hyper link
                if (i >= 5000) {
                    break;
                }
                MetaResultBean rb = new MetaResultBean(ids[i]);
                rb.setName(ids[i]);
                rb.setValue("ID", ids[i]);
                for (int m = 0; m < columnKeys.length; m++) {
                    rb.setValue(columnKeys[m], resMat[i][m] + "");
                }

                if (i < mb.getCurrentDeNum(mb.getAnalMethod())) {
                    rb.setSignificant(true);
                } else {
                    rb.setSignificant(false);

                }
                resBeans.add(rb);
            }
        }
        if (indField == null) {
            indField = "fc";
        }
        if (sortField == null) {
            sortField = tableFields[tableFields.length - 1].getValue().toString();
            System.out.println("sortField==" + sortField);
        }
        if (sortOrder == null) {
            sortOrder = "asc";
        }
        resBeanMap.put(mb.getAnalMethod(), resBeans);
    }

    public Map<String, List<MetaResultBean>> getResBeanMap() {
        return resBeanMap;
    }

    public List<MetaResultBean> getResBeans() {
        return resBeans;
    }

    public List<ColumnModel> getColumns() {
        return columns;
    }

    private String sortField;

    public String getSortField() {
        return sortField;
    }

    public void setSortField(String sortField) {
        if (sortField != null) {
            this.sortField = sortField;
        }
    }
    private String sortOrder = "asc";

    public String getSortOrder() {
        return sortOrder;
    }

    public void setSortOrder(String sortOrder) {
        if (sortOrder != null) {
            this.sortOrder = sortOrder;
        }
    }
    private SelectItem[] tableFields;

    public SelectItem[] getTableFields() {
        return tableFields;
    }
    private MetaResultBean selectedFeature;

    public MetaResultBean getSelectedFeature() {
        return selectedFeature;
    }

    public void setSelectedFeature(MetaResultBean selectedData) {
        this.selectedFeature = selectedData;
        String geneID = selectedFeature.getID();
        RMetaUtils.plotSelectedFeature(sb, geneID, "png", 72);
        currentFeatureImg = sb.getCurrentUser().getRelativeDir() + File.separator + "meta_ft_" + geneID + ".png";
    }

    private String currentFeatureImg;

    public String getCurrentFeatureImg() {
        return currentFeatureImg;
    }

    private String indField = "fc";

    public String getIndField() {
        return indField;
    }

    public void setIndField(String indField) {
        if (indField != null && !this.indField.equals(indField)) {
            this.indField = indField;
            populateResBeans();
        }
    }

    //sort table based on user selected field and order
    public void updateTable() {

        //first found it is number or string
        String type = "string";
        for (int k = 0; k < columns.size(); k++) {
            ColumnModel cm = columns.get(k);
            if (cm.getHeader().equals(sortField)) {
                type = cm.getType();
                break;
            }
        }
        if (type.equalsIgnoreCase("string")) {
            if (sortField.equalsIgnoreCase("ID")) {
                Collections.sort(resBeans, MyLinkCOMPARATOR);
            } else {
                Collections.sort(resBeans, MyStringCOMPARATOR);
            }
        } else {
            Collections.sort(resBeans, MyNumberCOMPARATOR);
        }
    }

    //String compare
    private Comparator<MetaResultBean> MyStringCOMPARATOR = new Comparator<MetaResultBean>() {
        @Override
        public int compare(MetaResultBean rb1, MetaResultBean rb2) {
            try {
                Object value1 = rb1.getValue(sortField);
                Object value2 = rb2.getValue(sortField);
                int value = ((Comparable) value1).compareTo(value2);
                return sortOrder.equals("asc") ? value : -1 * value;
            } catch (Exception e) {
                throw new RuntimeException();
            }
        }
    };
    //hyerlink then use its value ID
    private Comparator<MetaResultBean> MyLinkCOMPARATOR = new Comparator<MetaResultBean>() {
        @Override
        public int compare(MetaResultBean rb1, MetaResultBean rb2) {
            try {
                Object value1 = rb1.getName();
                Object value2 = rb2.getName();
                //System.out.println(value1 + " " + value2 + "========");
                int value = ((Comparable) value1).compareTo(value2);
                return sortOrder.equals("asc") ? value : -1 * value;
            } catch (Exception e) {
                throw new RuntimeException();
            }
        }
    };
    private final Comparator<MetaResultBean> MyNumberCOMPARATOR = new Comparator<MetaResultBean>() {
        @Override
        public int compare(MetaResultBean rb1, MetaResultBean rb2) {
            try {
                Double value1 = Double.valueOf(rb1.getValue(sortField));
                Double value2 = Double.valueOf(rb2.getValue(sortField));
                int value = value1.compareTo(value2);
                return sortOrder.equals("asc") ? value : -1 * value;
            } catch (Exception e) {
                throw new RuntimeException();
            }
        }
    };

    private String searchTerm = "";

    public String getSearchTerm() {
        return searchTerm;
    }

    public void setSearchTerm(String searchTerm) {
        this.searchTerm = searchTerm;
    }

    public void searchTable() {
        searchTerm = searchTerm.trim();

        int index = -1;
        for (int i = 0; i < resBeans.size(); i++) {
            if (resBeans.get(i).getName().equalsIgnoreCase(searchTerm)) {
                index = i;
                break;
            }
        }
        if (index == -1) {
            sb.addMessage("warn", "Make sure the feature name is spelled correctly.");
        } else {
            MetaResultBean res = resBeans.get(index);
            resBeans.remove(index);
            resBeans.add(0, res);
            sb.addMessage("info", "The feature is found - ranked first in the table.");
        }
    }

    public void onTabChange(TabChangeEvent event) {
        MetaLoadBean mb = (MetaLoadBean) DataUtils.findBean("loadBean");

        selectedKey = getKeyFromTableName(event.getTab().getTitle());         // tab title == key
        mb.setAnalMethod(selectedKey);
        populateResBeans();
        System.out.println("selectedKey===" + selectedKey);
    }

    public static String getTableName(String key) {
        return switch (key) {

            case "metap" ->
                "Combine P-values";
            case "votecount" ->
                "Vote Counting";
            case "merge" ->
                "Direct Merging";
            default -> {
                yield key;
            }
        };
    }

    public static String getKeyFromTableName(String tableName) {
        return switch (tableName) {
            case "Combine P-values" ->
                "metap";
            case "Vote Counting" ->
                "votecount";
            case "Direct Merging" ->
                "merge";
            default ->
                tableName;
        };
    }
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.enrich;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.Serializable;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.model.SelectItem;
import jakarta.faces.model.SelectItemGroup;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.REnrichUtils;
import pro.metaboanalyst.rwrappers.RIntegUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.inject.Inject;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author qiang
 */
@SessionScoped
@Named("integProcesser")
@JsonIgnoreProperties(ignoreUnknown = true)
public class IntegProcessBean implements Serializable {

    // Section I: Find other bean
    @JsonIgnore
    @Inject
    ApplicationBean1 ab;
    @JsonIgnore
    @Inject
    SessionBean1 sb;

    // Section II: variable
    private String datatype = "cmp";
    private double pvalCutoff = 0.01;
    private boolean disabledMumPval = false;
    private boolean loggedIn = false;
    private boolean cmpdMapped = false;
    private boolean geneMapped = false;
    private String integOrg = "hsa";
    private String geneList;
    private String cmpdList;
    private boolean useExample = false;
    private String msModeOpt = "negative";
    private double ppmVal = 5.0;
    private String RTOpt = "no";
    private String RankOpt = "pvalue";
    private boolean ECOpt = false;
    private UploadedFile peakFile;
    private String geneIDType = "NA";
    private double rtFrac = 0.02;
    private String version = "v1";
    private final String libVersion = "current";

    //Section II: General Data Process
    public String doJointSubmit1() {

        if (!(geneList == null | geneList.trim().length() == 0)) {
            if (geneIDType.equals("NA")) {
                sb.addMessage("Error", "Please specify gene ID type!");
                return null;
            }
            handleGeneListUpload();
            if (!geneMapped) {
                sb.addMessage("Error", "No gene hits!");
                return null;
            }
        }

        if (!(cmpdList == null | cmpdList.trim().length() == 0)) {
            if (sb.getCmpdIDType().equals("na")) {
                sb.addMessage("Error", "Please specify compound ID type!");
                return null;
            }
            handleCmpdListUpload();
        }

        if (!geneMapped && !cmpdMapped) {
            sb.addMessage("Error", "Please enter valid input!");
            return null;
        }

        if (!geneMapped && cmpdMapped) {
            sb.addMessage("Error", "Please use Pathway Analysis module for only metabolite list!");
            return null;
        }
        sb.setDataUploaded();
        return "ID map";

    }

    public String doJointSubmit2() {

        // Process Genes first        
        if (!(geneList == null | geneList.trim().length() == 0)) {
            if (geneIDType.equals("NA")) {
                sb.addMessage("Error", "Please specify gene ID type!");
                return null;
            }
            handleGeneListUpload();
            if (!geneMapped) {
                sb.addMessage("Error", "No gene hits!");
                return null;
            }
        }

        // Process Cmpds then
        String fileName;

        if (useExample) {
            fileName = ab.getInternalData("integ_peaks.txt");
        } else {
            try {
                if (peakFile == null) {
                    sb.addMessage("Error", "Please upload your peak file!");
                    return null;
                }
                if (peakFile.getSize() == 0) {
                    sb.addMessage("Error", "File is empty!");
                    return null;
                }
                fileName = DataUtils.getJustFileName(peakFile.getFileName());
                DataUtils.uploadFile(peakFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            } catch (Exception e) {
                return null;
            }

        }

        sb.setDataUploaded();
        RConnection RC = sb.getRConnection();
        RDataUtils.setPeakFormat(RC, RankOpt);
        RDataUtils.setInstrumentParams(RC, ppmVal, msModeOpt, ECOpt ? "yes" : "no", rtFrac);

        if (RDataUtils.readPeakListData(RC, fileName)) {
            MummiAnalBean mb = (MummiAnalBean) DataUtils.findBean("mummiAnalBean");
            String format = RDataUtils.GetPeakFormat(RC);
            switch (format) {
                case "mpt", "mprt" -> {
                    mb.setDisabledGsea(false);
                    mb.setDisabledMum(false);
                }
                case "mp", "mpr" -> {
                    mb.setDisabledGsea(true);
                    mb.setDisabledMum(false);
                }
                case "rmp" -> {
                    // single column
                    mb.setDisabledGsea(true);
                    mb.setDisabledMum(false);
                    mb.setDisabledMumPval(true);
                }
                default -> {
                    //rmt or mt or mtr
                    mb.setDisabledGsea(false);
                    mb.setDisabledMum(true);
                    mb.setDisabledMumPval(true);
                }
            }
            if (format.equals("mprt") || format.equals("mpr") || format.equals("mrt")) {
                mb.setDisabledV2(true);
            }
            return "ID map";
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "Failed to read in the peak list file." + err);
            return null;
        }
    }

    public String jointProceed() {
        String returnPage = null;

        if (!loggedIn) {
            if (!sb.doLogin("conc", "pathinteg", false, false)) {
                sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
                return null;
            } else {
                loggedIn = true;
                sb.initNaviTree("pathinteg");
                returnPage = "integlibs";
            }
        }

        return returnPage;
    }

    public String jointSubmit() {
        String returnPage = null;
        if (integOrg.equals("NA")) {
            sb.addMessage("Error", "Please choose an organism!");
            return null;
        }

        RDataUtils.setOrganism(sb.getRConnection(), integOrg);
        if (datatype.equals("cmp")) {
            returnPage = doJointSubmit1();
        } else if (datatype.equals("peak")) {
            returnPage = doJointSubmit2();
        }

        return returnPage;
    }

    private void handleCmpdListUpload() {

        cmpdList = cmpdList.trim();
        cmpdMapped = false;
        RConnection RC = sb.getRConnection();
        int res = RIntegUtils.performCmpdMapping(RC, cmpdList, integOrg, sb.getCmpdIDType());

        String info[] = RDataUtils.getNameCheckMsgs(RC);
        // int state = Integer.parseInt(info[0]);
        String msg = info[1];

        if (res == 1) {
            sb.addMessage("OK", msg);
            cmpdMapped = true;
        } else {
            sb.addMessage("Error", msg);
        }
        //cmpdList = null;
    }

    private void handleGeneListUpload() {

        geneList = geneList.trim();
        geneMapped = false;
        RConnection RC = sb.getRConnection();
        int res = RIntegUtils.performGeneMapping(RC, geneList, integOrg, geneIDType);
        if (res == 1) {
            sb.addMessage("OK", RDataUtils.getCurrentMsg(RC));
            geneMapped = true;
        } else {
            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
        }
        //geneList = null;
    }

    //Section III: Mummichog function
    public int PerformMummiInitPrediction() {
        if (ECOpt) {
            version = "v2";
        }
        return PerformMummiCore();
    }

    private int PerformMummiCore() {
        RConnection RC = sb.getRConnection();
        RDataUtils.setPeakEnrichMethod(RC, "mum", version);
        int minMsetNum = 3;
        String pathDBOpt = integOrg + "_kegg";
        if (REnrichUtils.setupMummichogPval(RC, pvalCutoff)) {
            if (!REnrichUtils.performPSEA(RC, pathDBOpt, libVersion, minMsetNum)) {
                String msg = RDataUtils.getErrMsg(sb.getRConnection());
                sb.addMessage("Error", "There is something wrong with the MS Peaks to Paths analysis: " + msg);
                return 0;
            }
        } else {
            String msg = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", msg + "You can click the Submit button again to accept recommended p value.");
            return 0;
        }
        return 1;
    }

    public double findPvalue() {
        double pcutoff;
        pcutoff = REnrichUtils.getDefaultPvalCutoff(sb.getRConnection());
        if (pcutoff == -1) {
            pcutoff = 0.01;
        }
        return pcutoff;
    }

    public String getCmpTabTitle() {
        if (datatype.equals("peak")) {
            return "Peaks Processing";
        }
        return "Compound Name Mapping";
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public double getRtFrac() {
        return rtFrac;
    }

    public void setRtFrac(double rtFrac) {
        this.rtFrac = rtFrac;
    }

    public String getCmpdList() {
        return cmpdList;
    }

    public void setCmpdList(String cmpdList) {
        this.cmpdList = cmpdList;
    }

    public boolean isCmpdMapped() {
        return cmpdMapped;
    }

    public void setCmpdMapped(boolean cmpdMapped) {
        this.cmpdMapped = cmpdMapped;
    }

    public boolean isGeneMapped() {
        return geneMapped;
    }

    public void setGeneMapped(boolean geneMapped) {
        this.geneMapped = geneMapped;
    }

    public String getGeneIDType() {
        return geneIDType;
    }

    public void setGeneIDType(String geneIDType) {
        this.geneIDType = geneIDType;
    }

    public String getGeneList() {
        return geneList;
    }

    public void setGeneList(String geneList) {
        this.geneList = geneList;
    }

    public boolean isLoggedIn() {
        return loggedIn;
    }

    public void setLoggedIn(boolean loggedIn) {
        this.loggedIn = loggedIn;
    }

    public String getIntegOrg() {
        return integOrg;
    }

    public void setIntegOrg(String integOrg) {
        this.integOrg = integOrg;
    }
    private List<SelectItem> speciesGroup;

    //@PostConstruct
    public void initOrgsMenu() {
        speciesGroup = new ArrayList<>();
        SelectItemGroup mammals = new SelectItemGroup("Mammals");
        mammals.setSelectItems(new SelectItem[]{
            new SelectItem("hsa", "Homo sapiens"),
            new SelectItem("ptr", "Pan troglodytes (chimpanzee)"),
            new SelectItem("mcc", "Macaca mulatta (rhesus monkey)"),
            new SelectItem("mcf", "Macaca fascicularis (crab-eating macaque)"),
            new SelectItem("mmu", "Mus musculus (house mouse)"),
            new SelectItem("rno", "Rattus norvegicus (rat)"),
            new SelectItem("ocu", "Oryctolagus cuniculus (rabbit)"),
            new SelectItem("cfa", "Canis lupus familiaris (dog)"),
            new SelectItem("fca", "Felis catus (domestic cat)"),
            new SelectItem("bta", "Bos taurus (cow)"),
            new SelectItem("biu", "Bos indicus (zebu cattle)"),
            new SelectItem("chx", "Capra hircus (goat)"),
            new SelectItem("oas", "Ovis aries (sheep)"),
            new SelectItem("ssc", "Sus scrofa (pig)"),
            new SelectItem("ecb", "Equus caballus (horse)"),
            new SelectItem("eai", "Equus asinus (ass)"),
            new SelectItem("hai", "Hipposideros armiger (great roundleaf bat)"),
            new SelectItem("dro", "Desmodus rotundus (common vampire bat)")
        });
        SelectItemGroup birds = new SelectItemGroup("Birds");
        birds.setSelectItems(new SelectItem[]{
            new SelectItem("gga", "Gallus gallus (chicken)"),
            new SelectItem("tgu", "Taeniopygia guttata (zebra finch)")
        });
        SelectItemGroup fish = new SelectItemGroup("Fish");
        fish.setSelectItems(new SelectItem[]{
            new SelectItem("dre", "Danio rerio (zebrafish)"),
            new SelectItem("nfu", "Nothobranchius furzeri (turquoise killifish)")
        });
        SelectItemGroup flatworms = new SelectItemGroup("Flatworms");
        flatworms.setSelectItems(new SelectItem[]{
            new SelectItem("smm", "Schistosoma mansoni"),
            new SelectItem("shx", "Schistosoma haematobium (urinary blood fluke)")
        });
        SelectItemGroup fungi = new SelectItemGroup("Fungus");
        fungi.setSelectItems(new SelectItem[]{
            new SelectItem("sce", "Saccharomyces cerevisiae (budding yeast)"),
            new SelectItem("cgr", "Nakaseomyces glabratus"),
            new SelectItem("ppa", "Komagataella phaffii"),
            new SelectItem("cal", "Candida albicans"),
            new SelectItem("pkz", "Pichia kudriavzevii"),
            new SelectItem("act", "Aspergillus clavatus"),
            new SelectItem("ang", "Aspergillus niger (black aspergilli)"),
            new SelectItem("afv", "Aspergillus flavus"),
            new SelectItem("cim", "Coccidioides immitis"),
            new SelectItem("cne", "Cryptococcus neoformans var. neoformans JEC21"),
            new SelectItem("cgi", "Cryptococcus gattii")
        });
        SelectItemGroup insects = new SelectItemGroup("Insects");
        insects.setSelectItems(new SelectItem[]{
            new SelectItem("dme", "Drosophila melanogaster (fruit fly)"),
            new SelectItem("dsr", "Drosophila serrata"),
            new SelectItem("dpe", "Drosophila persimilis"),
            new SelectItem("aara", "Anopheles arabiensis"),
            new SelectItem("aag", "Aedes aegypti (yellow fever mosquito)"),
            new SelectItem("ame", "Apis mellifera (honey bee)"),
            new SelectItem("bmor", "Bombyx mori (domestic silkworm)"),
            new SelectItem("api", "Acyrthosiphon pisum (pea aphid)"),
            new SelectItem("gmw", "Galleria mellonella (greater wax moth)")
        });
        // Nematodes
        SelectItemGroup nematodes = new SelectItemGroup("Nematodes");
        nematodes.setSelectItems(new SelectItem[]{
            new SelectItem("cel", "Caenorhabditis elegans (nematode)"),
            new SelectItem("bmy", "Brugia malayi (filaria)")
        });
        // Plants
        SelectItemGroup plants = new SelectItemGroup("Plants");
        plants.setSelectItems(new SelectItem[]{
            new SelectItem("ath", "Arabidopsis thaliana (thale cress)"),
            new SelectItem("ghi", "Gossypium hirsutum (upland cotton)"),
            new SelectItem("ahf", "Arachis hypogaea (peanut)"),
            new SelectItem("hbr", "Hevea brasiliensis (rubber tree)"),
            new SelectItem("osa", "Oryza sativa japonica (Japanese rice)"),
            new SelectItem("taes", "Triticum aestivum (bread wheat)"),
            new SelectItem("zma", "Zea mays (maize)"),
            new SelectItem("cre", "Chlamydomonas reinhardtii"),
            new SelectItem("cvr", "Chlorella variabilis"),
            new SelectItem("cme", "Cyanidioschyzon merolae"),
            new SelectItem("gmx", "Glycine max (soybean)")
        });
        // Prokaryotes
        SelectItemGroup prokaryotes = new SelectItemGroup("Prokaryotes");
        prokaryotes.setSelectItems(new SelectItem[]{
            new SelectItem("eco", "Escherichia coli K-12 MG1655"),
            new SelectItem("sty", "Salmonella enterica subsp. enterica serovar Typhi CT18"),
            new SelectItem("sdy", "Shigella dysenteriae Sd197"),
            new SelectItem("kpn", "Klebsiella pneumoniae subsp. pneumoniae MGH 78578 (serotype K52)"),
            new SelectItem("kva", "Klebsiella variicola At-22"),
            new SelectItem("ype", "Yersinia pestis CO92 (biovar Orientalis)"),
            new SelectItem("smar", "Serratia marcescens SM39"),
            new SelectItem("hin", "Haemophilus influenzae Rd KW20 (serotype d)"),
            new SelectItem("vch", "Vibrio cholerae O1 El Tor N16961"),
            new SelectItem("vcf", "Vibrio cholerae O1 El Tor FJ147"),
            new SelectItem("vpa", "Vibrio parahaemolyticus RIMD 2210633"),
            new SelectItem("pae", "Pseudomonas aeruginosa PAO1"),
            new SelectItem("ppu", "Pseudomonas putida KT2440"),
            new SelectItem("lpn", "Legionella pneumophila subsp. pneumophila Philadelphia 1 (serogroup 1)"),
            new SelectItem("lha", "Legionella hackeliae"),
            new SelectItem("ftu", "Francisella tularensis subsp. tularensis SCHU S4"),
            new SelectItem("fper", "Francisella persica"),
            new SelectItem("nme", "Neisseria meningitidis MC58 (serogroup B)"),
            new SelectItem("ngo", "Neisseria gonorrhoeae FA 1090"),
            new SelectItem("bma", "Burkholderia mallei ATCC 23344"),
            new SelectItem("bpe", "Bordetella pertussis Tohama I"),
            new SelectItem("mlo", "Mesorhizobium japonicum MAFF 303099"),
            new SelectItem("rle", "Rhizobium johnstonii"),
            new SelectItem("bmf", "Brucella abortus 2308"),
            new SelectItem("bov", "Brucella ovis"),
            new SelectItem("aace", "Acetobacter aceti"),
            new SelectItem("hpy", "Helicobacter pylori 26695"),
            new SelectItem("hbi", "Helicobacter bizzozeronii"),
            new SelectItem("bsu", "Bacillus subtilis subsp. subtilis 168"),
            new SelectItem("ban", "Bacillus anthracis Ames"),
            new SelectItem("sau", "Staphylococcus aureus subsp. aureus N315 (MRSA/VSSA)"),
            new SelectItem("sha", "Staphylococcus haemolyticus JCSC1435"),
            new SelectItem("lmo", "Listeria monocytogenes EGD-e (serotype 1/2a)"),
            new SelectItem("liv", "Listeria ivanovii subsp. ivanovii PAM 55"),
            new SelectItem("spym", "Streptococcus pyogenes M1 476 (serotype M1)"),
            new SelectItem("spn", "Streptococcus pneumoniae TIGR4 (virulent serotype 4)"),
            new SelectItem("ctc", "Clostridium tetani E88"),
            new SelectItem("mtu", "Mycobacterium tuberculosis H37Rv"),
            new SelectItem("mle", "Mycobacterium leprae TN"),
            new SelectItem("nod", "Nocardia otitidiscaviarum"),
            new SelectItem("nad", "Nocardia asteroides"),
            new SelectItem("sals", "Streptomyces albus"),
            new SelectItem("blo", "Glycine max (soybean)"),
            new SelectItem("syf", "Synechococcus elongatus PCC7942"),
            new SelectItem("pma", "Prochlorococcus marinus subsp. marinus CCMP1375"),
            new SelectItem("tma", "Thermotoga maritima MSB8"),
            new SelectItem("lpx", "Lactiplantibacillus paraplantarum L-ZS9"),
            new SelectItem("wpa", "Weissella paramesenteroides FDAARGOS_414"),
            new SelectItem("wcb", "Weissella cibaria CH2"),
            new SelectItem("lpl", "Lactiplantibacillus plantarum WCFS1"),
            new SelectItem("lsa", "Latilactobacillus sakei subsp. sakei 23K"),
            new SelectItem("lla", "Lactococcus lactis subsp. lactis Il1403"),
            new SelectItem("lrg", "Lacticaseibacillus rhamnosus GG"),
            new SelectItem("saa", "Staphylococcus aureus subsp. aureus USA300FPR3757 (CA-MRSA)")
        });
        // Protists
        SelectItemGroup protists = new SelectItemGroup("Protists");
        protists.setSelectItems(new SelectItem[]{
            new SelectItem("pfa", "Plasmodium falciparum 3D7"),
            new SelectItem("pfd", "Plasmodium falciparum Dd2"),
            new SelectItem("pfh", "Plasmodium falciparum HB3"),
            new SelectItem("prei", "Plasmodium reichenowi"),
            new SelectItem("pyo", "Plasmodium yoelii"),
            new SelectItem("pbe", "Plasmodium berghei"),
            new SelectItem("pkn", "Plasmodium knowlesi"),
            new SelectItem("pvx", "Plasmodium vivax"),
            new SelectItem("pcy", "Plasmodium cynomolgi"),
            new SelectItem("tgo", "Toxoplasma gondii"),
            new SelectItem("ptm", "Paramecium tetraurelia"),
            new SelectItem("tbr", "Trypanosoma brucei brucei"),
            new SelectItem("tcr", "Trypanosoma cruzi"),
            new SelectItem("lma", "Leishmania major"),
            new SelectItem("tva", "Trichomonas vaginalis"),
            new SelectItem("gla", "Giardia lamblia")
        });
        // Other animals
        SelectItemGroup otheranimals = new SelectItemGroup("Other Animals");
        otheranimals.setSelectItems(new SelectItem[]{
            new SelectItem("tsr", "Thamnophis sirtalis (common garter snake)"),
            new SelectItem("spu", "Strongylocentrotus purpuratus (purple sea urchin)"),
            new SelectItem("dpx", "Daphnia pulex (common water flea)")
        });
        speciesGroup.add(mammals);
        speciesGroup.add(birds);
        speciesGroup.add(fish);
        speciesGroup.add(flatworms);
        speciesGroup.add(fungi);
        speciesGroup.add(insects);
        speciesGroup.add(nematodes);
        speciesGroup.add(plants);
        speciesGroup.add(prokaryotes);
        speciesGroup.add(protists);
        speciesGroup.add(otheranimals);
        System.out.println("speciesGroupInfo len ===> " + speciesGroup.size());
    }

    public List<SelectItem> getSpeciesGroup() {
        return speciesGroup;
    }

    public void setSpeciesGroup(List<SelectItem> speciesGroup) {
        this.speciesGroup = speciesGroup;
    }

    private SelectItem[] pathLibOpt = null;

    private void setupPathLibOpt() {
        int rowLen = 130;
        pathLibOpt = new SelectItem[rowLen];
        String[] vals = {"hsa", "mmu", "rno", "ptr", "mcc", "mcf", "ocu", "cfa", "fca",
            "bta", "biu", "chx", "oas", "ssc", "ecb", "eai", "hai", "dro", "gga", "tgu",
            "dre", "nfu", "smm", "shx", "sce", "cgr", "ppa", "cai", "pkz", "act", "ang",
            "afv", "cim", "cne", "cgi", "dme", "dsr", "dpe", "aara", "aag", "ame", "bmor",
            "api", "gmw", "cel", "bmy", "ath", "ghi", "ahf", "hbf", "osa", "taes", "zma",
            "cre", "cvr", "cme", "gmx", "eco", "sty", "sdy", "kpn", "kva", "ype", "smar",
            "hin", "vch", "vcf", "vpa", "pae", "ppu", "lpn", "lha", "ftu", "fper", "nme",
            "ngo", "bma", "bpe", "mlo", "rle", "bmf", "bov", "aace", "hpy", "hbi", "bsu",
            "ban", "sau", "sha", "lmo", "liv", "spym", "spn", "ctc", "mtu", "mle", "nod",
            "nad", "sals", "blo", "syf", "pma", "tma", "lpx", "wpa", "wcb", "lpl", "lsa",
            "lla", "lrg", "saa", "pfa", "pfd", "pfh", "prei", "pyo", "pbe", "pkn", "pvx",
            "pcy", "tgo", "ptm", "tbr", "tcr", "lma", "tva", "gla", "tsr", "spu", "dpz"};
        for (int i = 0; i < rowLen; i++) {
            pathLibOpt[i] = new SelectItem(vals[i], vals[i]);
        }
    }

    public SelectItem[] getPathLibOpt() {
        if (pathLibOpt == null) {
            setupPathLibOpt();
        }
        return pathLibOpt;
    }

    public void setPathLibOpt(SelectItem[] pathLibOpt) {
        this.pathLibOpt = pathLibOpt;
    }

    public String getMsModeOpt() {
        return msModeOpt;
    }

    public void setMsModeOpt(String msModeOpt) {
        this.msModeOpt = msModeOpt;
    }

    public double getPpmVal() {
        return ppmVal;
    }

    public void setPpmVal(double ppmVal) {
        this.ppmVal = ppmVal;
    }

    public String getRTOpt() {
        return RTOpt;
    }

    public void setRTOpt(String RTOpt) {
        this.RTOpt = RTOpt;
    }

    public String getRankOpt() {
        return RankOpt;
    }

    public void setRankOpt(String RankOpt) {
        this.RankOpt = RankOpt;
    }

    public boolean isECOpt() {
        return ECOpt;
    }

    public void setECOpt(boolean ECOpt) {
        this.ECOpt = ECOpt;
    }

    public UploadedFile getPeakFile() {
        return peakFile;
    }

    public void setPeakFile(UploadedFile peakFile) {
        this.peakFile = peakFile;
    }

    public boolean isUseExample() {
        return useExample;
    }

    public void setUseExample(boolean useExample) {
        this.useExample = useExample;
    }

    public boolean isDisabledMumPval() {
        return disabledMumPval; //TODO: disable working for mz single list
    }

    public void setDisabledMumPval(boolean disabledMumPval) {
        this.disabledMumPval = disabledMumPval;
    }

    public double getPvalCutoff() {
        pvalCutoff = findPvalue();
        return pvalCutoff;
    }

    public void setPvalCutoff(double pvalCutoff) {
        this.pvalCutoff = pvalCutoff;
    }

    public String getDatatype() {
        return datatype;
    }

    public void setDatatype(String datatype) {
        this.datatype = datatype;
    }

}

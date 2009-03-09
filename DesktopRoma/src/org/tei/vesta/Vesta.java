package org.tei.vesta;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.tei.utils.XMLUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.cloudgarden.resource.SWTResourceManager;


/**
* This code was edited or generated using CloudGarden's Jigloo
* SWT/Swing GUI Builder, which is free for non-commercial
* use. If Jigloo is being used commercially (ie, by a corporation,
* company or business for any purpose whatever) then you
* should purchase a license for each developer using Jigloo.
* Please visit www.cloudgarden.com for details.
* Use of Jigloo implies acceptance of these licensing terms.
* A COMMERCIAL LICENSE HAS NOT BEEN PURCHASED FOR
* THIS MACHINE, SO JIGLOO OR THIS CODE CANNOT BE USED
* LEGALLY FOR ANY CORPORATE OR COMMERCIAL PURPOSE.
*/
public class Vesta extends org.eclipse.swt.widgets.Composite {

	private String selectedFileName;
	private String selectedOutputDir;
	
	private boolean oddFileLoaded = true;
	
	private Menu menu1;
	private CLabel selectedOutputDirLabel;
	private CLabel selectedInputFileLabel;
	private Button goButton;
	private Button selectOutputDir;
	private Button selectInputFile;
	private Button cbCreateXSD;
	private List availableFormatsList_oddTrans;
	private Label selectFormatsLabel_oddTrans;
	private Combo selectProfileCombo_oddTrans;
	private Label selectProfileLabel_oddTrans;
	private Label selectFormatLabel;
	private List availableFormatsList_xmlTrans;
	private Combo selectProfileCombo_xmlTrans;
	private Label selectProfileLabel_xmlTrans;
	private Composite teiConvertOptionsComposite;
	private TabItem teiConvertOptions;
	private Label selectLanguageLabel;
	private Label imageLabel;
	private Label schemaToGenerateLabel;
	private Label labelPatternPrefix;
	private Text textPatternPrefix;
	private Button cbParameterized;
	private Button cbTEIVersion;
	private Button cbDebug;
	private Button cbCompile;
	private Composite composite1;
	private Button cbDocTEI;
	private Button cbCreateRNG;
	private Button cbCreateDTD;
	private Composite optionsTabComposite;
	private Group InputOutputGroup;
	private TabItem advOptionsTab;
	private TabItem oddFileOptionsTab;
	private TabFolder optionsTabFolder;
	private Combo combLanguage;
	private Combo combSchema;
	private String baseDir;

	{
		//Register as a resource user - SWTResourceManager will
		//handle the obtaining and disposing of resources
		SWTResourceManager.registerResourceUser(this);
	}

	public Vesta(Composite parent, int style) {
		
		super(parent, style);
		
		baseDir = Vesta.class.getProtectionDomain().getCodeSource().getLocation().getPath();
		baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		File baseDirFile = new File(baseDir + File.separator + "resources");
		if(!baseDirFile.exists())
			baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		
		baseDir += File.separator;
		
		try {
			baseDir = URLDecoder.decode(baseDir,"UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		
		initGUI();
	}
	
	/**
	* Initializes the GUI.
	*/
	private void initGUI() {
		try {
			this.setSize(525, 334);
			this.setLayout(null);
			{
				optionsTabFolder = new TabFolder(this, SWT.NONE);
				{
					teiConvertOptions = new TabItem(optionsTabFolder, SWT.NONE);
					teiConvertOptions.setText("Transformation Options");
					{
						teiConvertOptionsComposite = new Composite(optionsTabFolder, SWT.NONE);
						teiConvertOptionsComposite.setLayout(null);
						teiConvertOptions.setControl(teiConvertOptionsComposite);
						{
							selectProfileLabel_xmlTrans = new Label(teiConvertOptionsComposite, SWT.NONE);
							selectProfileLabel_xmlTrans.setText("select application:");
							selectProfileLabel_xmlTrans.setBounds(5, 10, 117, 19);
						}
						{
							availableFormatsList_xmlTrans = new List(teiConvertOptionsComposite, SWT.READ_ONLY | SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
							availableFormatsList_xmlTrans.setBounds(196, 28, 243, 91);
							availableFormatsList_xmlTrans.setEnabled(false);
						}
						{
							selectProfileCombo_xmlTrans = new Combo(teiConvertOptionsComposite, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.SINGLE);
							selectProfileCombo_xmlTrans.setBounds(0, 25, 159, 30);
							selectProfileCombo_xmlTrans.setEnabled(false);
							selectProfileCombo_xmlTrans.addSelectionListener(new SelectionAdapter() {
									public void widgetSelected(SelectionEvent evt){
										updateAvailableFormats(availableFormatsList_xmlTrans, selectProfileCombo_xmlTrans);
									}
							});

							loadProfiles(selectProfileCombo_xmlTrans, availableFormatsList_xmlTrans);
						}
						{
							selectFormatLabel = new Label(teiConvertOptionsComposite, SWT.NONE);
							selectFormatLabel.setText("select output formats:");
							selectFormatLabel.setBounds(196, 10, 138, 30);
						}
					}
				}
				{
					if(PropertiesProvider.getInstance().isOddEnabled()){
						oddFileOptionsTab = new TabItem(optionsTabFolder, SWT.NONE);
						oddFileOptionsTab.setText("Odd File Options");
						{
							optionsTabComposite = new Composite(optionsTabFolder, SWT.NONE);
							oddFileOptionsTab.setControl(optionsTabComposite);
							optionsTabComposite.setLayout(null);
							{
								cbCreateXSD = new Button(optionsTabComposite, SWT.CHECK | SWT.LEFT);
								cbCreateXSD.setText("generate XSD");
								cbCreateXSD.setBounds(182, 36, 114, 30);
								cbCreateXSD.setSelection(true);
								cbCreateXSD.setToolTipText("Generate a W3C Schema");
								cbCreateXSD.setEnabled(false);
							}
							{
								combLanguage = new Combo(optionsTabComposite, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.SINGLE );
								combLanguage.setBounds(0, 12, 159, 30);
								combLanguage.setToolTipText("put a label saying \"language for documentation\"");
								combLanguage.setEnabled(false);
								combLanguage.add("English");
								combLanguage.add("Español");
								combLanguage.add("Deutsch");
								combLanguage.add("Français");
								combLanguage.add("Italiano");
								combLanguage.add("日本語");
								combLanguage.add("中文");
								combLanguage.select(0);
							}
							{
								cbCreateRNG = new Button(optionsTabComposite, SWT.CHECK | SWT.LEFT);
								cbCreateRNG.setText("generate RNG");
								cbCreateRNG.setBounds(182, 0, 114, 30);
								cbCreateRNG.setSelection(true);
								cbCreateRNG.setToolTipText("Generate a RELAX NG schema in XML and compact format");
								cbCreateRNG.setEnabled(false);
							}
							{
								cbCreateDTD = new Button(optionsTabComposite, SWT.CHECK | SWT.LEFT);
								cbCreateDTD.setText("generate DTD");
								cbCreateDTD.setBounds(182, 72, 114, 30);
								cbCreateDTD.setSelection(true);
								cbCreateDTD.setToolTipText("Generate an XML DTD");
								cbCreateDTD.setEnabled(false);
							}
							{
								cbDocTEI = new Button(optionsTabComposite, SWT.CHECK | SWT.LEFT);
								cbDocTEI.setText("documentation (TEI)");
								cbDocTEI.setBounds(316, 0, 173, 30);
								cbDocTEI.setToolTipText("Create documentation for your schema, in TEI XML conforming to the TEI Lite schema");
								cbDocTEI.setEnabled(false);
							}
							{
								selectLanguageLabel = new Label(optionsTabComposite, SWT.NONE);
								selectLanguageLabel.setText("select language:");
								selectLanguageLabel.setBounds(6, 0, 113, 19);
								selectLanguageLabel.setToolTipText("put a label saying \"language for documentation\"");
							}
							{
								schemaToGenerateLabel = new Label(optionsTabComposite, SWT.LEFT);
								schemaToGenerateLabel.setText("select schema:");
								schemaToGenerateLabel.setBounds(6, 42, 140, 20);
								schemaToGenerateLabel.setToolTipText("Choose which schema to process from your ODD file");
							}
							{
								combSchema = new Combo(optionsTabComposite, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.SINGLE);
								combSchema.setBounds(0, 54, 159, 30);
								combSchema.setToolTipText("Choose which schema to process from your ODD file");
								combSchema.setEnabled(false);
							}
							{
								availableFormatsList_oddTrans = new List(optionsTabComposite, SWT.READ_ONLY | SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
								availableFormatsList_oddTrans.setBounds(316, 54, 167, 64);
								availableFormatsList_oddTrans.setEnabled(false);
							}
							{
								selectProfileLabel_oddTrans = new Label(optionsTabComposite, SWT.NONE);
								selectProfileLabel_oddTrans.setText("select profile:");
								selectProfileLabel_oddTrans.setBounds(6, 84, 129, 15);
							}
							{
								selectProfileCombo_oddTrans = new Combo(optionsTabComposite,  SWT.DROP_DOWN | SWT.READ_ONLY | SWT.SINGLE);
								selectProfileCombo_oddTrans.setBounds(0, 96, 159, 30);
								selectProfileCombo_oddTrans.setEnabled(false);
								selectProfileCombo_oddTrans.addSelectionListener(new SelectionAdapter() {
									public void widgetSelected(SelectionEvent evt){
										updateAvailableFormats(availableFormatsList_oddTrans, selectProfileCombo_oddTrans);
									}
								});
								
								loadProfiles(selectProfileCombo_oddTrans, availableFormatsList_oddTrans);
							}
							{
								selectFormatsLabel_oddTrans = new Label(optionsTabComposite, SWT.NONE);
								selectFormatsLabel_oddTrans.setText("documentation formats:");
								selectFormatsLabel_oddTrans.setBounds(316, 38, 167, 18);
							}
						}
					}
				}
				{
					if(PropertiesProvider.getInstance().isOddEnabled()){
						advOptionsTab = new TabItem(optionsTabFolder, SWT.NONE);
						advOptionsTab.setText("Advanced Odd File Options");
						{
							composite1 = new Composite(optionsTabFolder, SWT.NONE);
							advOptionsTab.setControl(composite1);
							composite1.setBounds(26, 52, 60, 30);
							composite1.setLayout(null);
							{
								cbCompile = new Button(composite1, SWT.CHECK | SWT.LEFT);
								cbCompile.setText("compile");
								cbCompile.setBounds(12, 0, 102, 30);
								cbCompile.setToolTipText("create intermediate representation of the ODD file after it has been merged with the main TEI");
								cbCompile.setEnabled(false);
							}
							{
								cbDebug = new Button(composite1, SWT.CHECK | SWT.LEFT);
								cbDebug.setText("debug");
								cbDebug.setBounds(12, 36, 102, 30);
								cbDebug.setToolTipText("produce verbose messages");
								cbDebug.setEnabled(false);
							}
							{
								cbTEIVersion = new Button(composite1, SWT.CHECK | SWT.LEFT);
								cbTEIVersion.setText("use version from TEI");
								cbTEIVersion.setBounds(146, 0, 168, 30);
								cbTEIVersion.setSelection(true);
								cbTEIVersion.setToolTipText("use the date and version of the current TEI Guidelines in documentation, rather than the date and version in your ODD");
								cbTEIVersion.setEnabled(false);
							}
							{
								cbParameterized = new Button(composite1, SWT.CHECK | SWT.LEFT);
								cbParameterized.setText("parameterized DTD");
								cbParameterized.setBounds(326, 0, 157, 30);
								cbParameterized.setToolTipText("create a DTD suitable for customizing in a document DTD subset");
								cbParameterized.setEnabled(false);
							}
							{
								textPatternPrefix = new Text(composite1, SWT.NONE);
								textPatternPrefix.setBounds(242, 42, 230, 22);
								textPatternPrefix.setText("tei_");
								textPatternPrefix.setToolTipText("name all RELAX NG patterns with a name prefix to avoid clashes with other vocabularies");
								textPatternPrefix.setEnabled(false);
							}
							{
								labelPatternPrefix = new Label(composite1, SWT.LEFT);
								labelPatternPrefix.setText("pattern prefix:");
								labelPatternPrefix.setBounds(146, 45, 90, 17);
								labelPatternPrefix.setToolTipText("name all RELAX NG patterns with a name prefix to avoid clashes with other vocabularies");
							}
						}
					}
				}
				optionsTabFolder.setBounds(7, 120, 512, 169);
				optionsTabFolder.setSelection(0);
			}
			{
				goButton = new Button(this, SWT.PUSH | SWT.CENTER);
				goButton.setText("Go");
				goButton.setBounds(409, 301, 104, 32);
				goButton.addSelectionListener(new SelectionAdapter() {
					public void widgetSelected(SelectionEvent evt) {
						goButtonWidgetSelected(evt);
					}
				} );
			}
			{
				InputOutputGroup = new Group(this, SWT.NONE);
				InputOutputGroup.setLayout(null);
				InputOutputGroup.setText("");
				InputOutputGroup.setBounds(124, 14, 395, 89);
				{
					selectInputFile = new Button(InputOutputGroup, SWT.PUSH | SWT.CENTER);
					selectInputFile.setText("Select Input File");
					selectInputFile.setBounds(9, 7, 143, 32);
					selectInputFile.setToolTipText("Choose a TEI ODD specification");
					selectInputFile.addSelectionListener(new SelectionAdapter() {
						public void widgetSelected(SelectionEvent evt) {
							FileDialog fileDialog = new FileDialog(getShell());
							if(PropertiesProvider.getInstance().isOddEnabled())
								fileDialog.setFilterExtensions(new String[]{ "*.xml", "*.odd"});
							else
								fileDialog.setFilterExtensions(new String[]{ "*.xml" });
							selectedFileName = fileDialog.open();
							if(null == selectedFileName)
								return;
							selectedInputFileLabel.setText(selectedFileName);
							
							// read input file in DomDocument and search for schemas
							Document doc = null;
							try {
								doc = XMLUtils.readInputFileIntoJAXPDoc(new File(selectedFileName));
								NodeList list = doc.getDocumentElement().getElementsByTagName("schemaSpec");
								
								if(list.getLength() > 0){
									java.util.List<String> idents = new ArrayList<String>();
									for(int i = 0; i < list.getLength(); i++){
										idents.add(((Element)list.item(0)).getAttribute("ident"));
									}
									combSchema.setItems(idents.toArray(new String[]{}));
									combSchema.select(0);
								} else {
									throw new IllegalArgumentException("Could not find a schemaSpec in the input file.");
								}
								// enable potential disabled gui elements
								cbCreateDTD.setEnabled(true);
								cbCreateRNG.setEnabled(true);
								cbCreateXSD.setEnabled(true);
								cbCompile.setEnabled(true);
								cbDebug.setEnabled(true);
								cbDocTEI.setEnabled(true);
								combLanguage.setEnabled(true);
								cbParameterized.setEnabled(true);
								cbTEIVersion.setEnabled(true);
								textPatternPrefix.setEnabled(true);
								selectProfileCombo_oddTrans.setEnabled(true);
								availableFormatsList_oddTrans.setEnabled(true);
								combSchema.setEnabled(true);
								
								// disable gui elements
								selectProfileCombo_xmlTrans.setEnabled(false);
								availableFormatsList_xmlTrans.setEnabled(false);
								
								// chose odd file options
								optionsTabFolder.setSelection(1);
								
								
								// lets create a compiled odd
								oddFileLoaded = true;
							} catch (IllegalArgumentException e) {
								// if no schemaSpec was found .. deactivate everything but docx and html
								cbCreateDTD.setEnabled(false);
								cbCreateRNG.setEnabled(false);
								cbCreateXSD.setEnabled(false);
								cbCompile.setEnabled(false);
								cbDebug.setEnabled(false);
								cbDocTEI.setEnabled(false);
								combLanguage.setEnabled(false);
								cbParameterized.setEnabled(false);
								cbTEIVersion.setEnabled(false);
								textPatternPrefix.setEnabled(false);
								selectProfileCombo_oddTrans.setEnabled(false);
								availableFormatsList_oddTrans.setEnabled(false);
								combSchema.setEnabled(false);
								
								// lets not create a compiled odd but only allow for the transformation
								oddFileLoaded = false;
								
								selectProfileCombo_xmlTrans.setEnabled(true);
								availableFormatsList_xmlTrans.setEnabled(true);
								
								// select correct tab
								optionsTabFolder.setSelection(0);
								
								// preselect docX and HTML creation
							} catch (Exception e) {
								selectedFileName = null;
								selectedInputFileLabel.setText("");
								
								MessageBox mb = new MessageBox(Display.getCurrent().getActiveShell());
								mb.setMessage(e.getMessage());
								mb.open();
							}
							

							
						}
					});
				}
				{
					selectOutputDir = new Button(InputOutputGroup, SWT.PUSH | SWT.CENTER);
					selectOutputDir.setText("Select Output Dir ");
					selectOutputDir.setBounds(9, 45, 143, 32);
					selectOutputDir.setToolTipText("Choose a directory in which Vesta can create schemas and documentation");
					selectOutputDir.addSelectionListener(new SelectionAdapter() {
						public void widgetSelected(SelectionEvent evt) {
							DirectoryDialog dirDialog = new DirectoryDialog(getShell());
							selectedOutputDir = dirDialog.open();
							if(null == selectedOutputDir)
								return;
							selectedOutputDirLabel.setText(selectedOutputDir);
						}
					});
				}
				{
					selectedInputFileLabel = new CLabel(InputOutputGroup, SWT.NONE);
					selectedInputFileLabel.setBounds(158, 9, 226, 30);
					selectedInputFileLabel.setBackground(SWTResourceManager.getColor(203, 203, 203));
					selectedInputFileLabel.setToolTipText("Choose a TEI ODD specification");
				}
				{
					selectedOutputDirLabel = new CLabel(InputOutputGroup, SWT.NONE);
					selectedOutputDirLabel.setBounds(158, 45, 226, 30);
					selectedOutputDirLabel.setBackground(SWTResourceManager.getColor(203, 203, 203));
					selectedOutputDirLabel.setToolTipText("Choose a directory in which Vesta can create schemas and documentation");
				}
			}
			{
			}
			{
				imageLabel = new Label(this, SWT.NONE);
				try{
					Image image = new Image(Display.getDefault(), baseDir + "resources/images/TEI.png");
					if(null != image){
						imageLabel.setImage(image);
					}
					imageLabel.setBounds(14, 14, 100, 89);
				} catch(Exception e){
					System.out.println(e.getMessage());
					e.printStackTrace();
				}
			}
			{
				menu1 = new Menu(getShell(), SWT.BAR);
				getShell().setMenuBar(menu1);
			}
			this.layout();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void loadProfiles(Combo profileCombo, List formatsList) {
		File applicationsDir = new File(PropertiesProvider.getInstance().getStylesheetDir() + File.separator + "profiles");
		if(applicationsDir.isDirectory()){
			profileCombo.add("default");
			profileCombo.select(0);
			updateAvailableFormats(formatsList, profileCombo);
			for(File dir : applicationsDir.listFiles()){
				if(dir.isDirectory() && ! dir.getName().startsWith(".") && ! dir.getName().equals("default")){
					profileCombo.add(dir.getName());
				}
			}
		}
		
	}

	private void updateAvailableFormats(List formatList, Combo combo){
		formatList.removeAll();
		File dir = new File(PropertiesProvider.getInstance().getStylesheetDir() + File.separator + "profiles" + File.separator + combo.getText());
		if(dir.isDirectory()){
			for(File format: dir.listFiles()){
				if(! format.getName().startsWith(".") && format.isDirectory()){
					for(File file : format.listFiles()){
						if(file.getName().equals("to.xsl")){
							formatList.add(format.getName());
							break;
						}
					}
				}
			}
		}
	}
	
	/**
	* Auto-generated main method to display this 
	* org.eclipse.swt.widgets.Composite inside a new Shell.
	*/
	public static void main(String[] args) {
		try{
			Display.setAppName("Vesta");
			Display display = Display.getDefault();
			Shell shell = new Shell(display,SWT.DIALOG_TRIM);
			shell.setText("TEI Vesta");
			Vesta inst = new Vesta(shell, SWT.NULL);
			Point size = inst.getSize();
			
			shell.setLayout(new FillLayout());
			shell.layout();
			
			if(size.x == 0 && size.y == 0) {
				inst.pack();
				shell.pack();
			} else {
				Rectangle shellBounds = shell.computeTrim(0, 0, size.x, size.y);
				shell.setSize(shellBounds.width, shellBounds.height);
			}
			shell.open();
			while (!shell.isDisposed()) {
				if (!display.readAndDispatch())
					display.sleep();
			}
		} catch(Exception e){
			e.printStackTrace();
		}
	}

	public Button getCbCreateRNG() {
		return cbCreateRNG;
	}
	
	public Button getCbCreateXSD() {
		return cbCreateXSD;
	}
	
	public Button getCbCreateDTD() {
		return cbCreateDTD;
	}
	
	public Combo getCombLanguage() {
		return combLanguage;
	}
	
	public Button getCbDocTEI() {
		return cbDocTEI;
	}
	
	public Button getCbCompile() {
		return cbCompile;
	}
	
	public Button getCbDebug() {
		return cbDebug;
	}
	
	public Button getCbTEIVersion() {
		return cbTEIVersion;
	}
	
	public Text getTextPatternPrefix() {
		return textPatternPrefix;
	}
	
	public Button getCbParameterized() {
		return cbParameterized;
	}
	
	private void goButtonWidgetSelected(SelectionEvent evt) {
		// test that input and output parameters are set
		if  ( null == selectedFileName ||
			  null == selectedOutputDir ||
			  selectedFileName.equals("") || 
			  selectedOutputDir.equals("")){
			MessageBox mb = new MessageBox(getShell());
			mb.setMessage("Please select input file and output directory");
			mb.open();
			return;
		}
		
		// start up processor
		final VestaProcessor proc = new VestaProcessor();
		proc.setOddFile(selectedFileName);
		proc.setOutputDir(selectedOutputDir);
		proc.setUseCompiledODD(oddFileLoaded);
		if(oddFileLoaded){
			proc.setSchemaName(combSchema.getItem(combSchema.getSelectionIndex()));
			
			proc.setGenerateDTD(getCbCreateDTD().getSelection());
			proc.setGenerateRNG(getCbCreateRNG().getSelection());
			proc.setGenerateXSD(getCbCreateXSD().getSelection());
			
			// set language
			switch( getCombLanguage().getSelectionIndex() ){
			case 0:
				proc.setLanguage("en");
				break;
			case 1:
				proc.setLanguage("es");
				break;
			case 2:
				proc.setLanguage("de");
				break;
			case 3:
				proc.setLanguage("fr");
				break;
			case 4:
				proc.setLanguage("it");
				break;
			case 5:
				proc.setLanguage("ja");
				break;
			case 6:
				proc.setLanguage("zh-tw");
				break;
			}
			
			// formats
			proc.setDocumentationTEI(getCbDocTEI().getSelection());

			// advanced options
			proc.setCompile(getCbCompile().getSelection());
			proc.setDebug(getCbDebug().getSelection());
			proc.setUseVersionFromTEI(getCbTEIVersion().getSelection());
			proc.setParameterizedDTD(getCbParameterized().getSelection());
			proc.setPatternPrefix(getTextPatternPrefix().getText());
			
			// formats
			Collection<String> formats = new HashSet<String>();
			for(String f : availableFormatsList_oddTrans.getSelection())
				formats.add(f);
			proc.setFormats(formats);
			
			// application
			proc.setProfile(selectProfileCombo_oddTrans.getText());
			
		} else {
			// no odd file
			String name =new File(selectedFileName).getName();
			if(name.indexOf(".") != -1)
				name = name.substring(0, name.indexOf("."));
			proc.setSchemaName(name);

			// formats
			Collection<String> formats = new HashSet<String>();
			for(String f : availableFormatsList_xmlTrans.getSelection())
				formats.add(f);
			proc.setFormats(formats);
			
			// application
			proc.setProfile(selectProfileCombo_xmlTrans.getText());
		}
		
	
		Thread procThread = new Thread(proc);
		procThread.start();
	}

}

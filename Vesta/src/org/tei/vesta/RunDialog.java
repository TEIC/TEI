package org.tei.vesta;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

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
public class RunDialog extends org.eclipse.swt.widgets.Dialog {

	private Shell dialogShell;
	private Button closeButton;
	private Text messagesText;
	private boolean allowClose = false;

	public RunDialog(Shell parent, int style) {
		super(parent, style);
	}

	public void open() {
		try {
			Shell parent = getParent();
			dialogShell = new Shell(parent, SWT.RESIZE | SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);

			FormLayout dialogShellLayout = new FormLayout();
			dialogShell.setLayout(dialogShellLayout);
			dialogShell.layout();
			dialogShell.pack();			
			dialogShell.setSize(518, 286);
			
			dialogShell.setText("Messages");
			{
				closeButton = new Button(dialogShell, SWT.PUSH | SWT.CENTER);
				FormData closeButtonLData = new FormData();
				closeButtonLData.width = 70;
				closeButtonLData.height = 32;
				closeButtonLData.bottom =  new FormAttachment(1000, 1000, -3);
				closeButtonLData.right =  new FormAttachment(1000, 1000, -3);
				closeButton.setLayoutData(closeButtonLData);
				closeButton.setText("Close");
				closeButton.addSelectionListener(new SelectionAdapter() {
					public void widgetSelected(SelectionEvent evt) {
						dialogShell.close();
					}
				});
			}
			{
				FormData messagesTextLData = new FormData();
				messagesTextLData.width = 503;
				messagesTextLData.height = 214;
				messagesTextLData.top =  new FormAttachment(0, 1000, 0);
				messagesTextLData.left =  new FormAttachment(0, 1000, 0);
				messagesTextLData.right =  new FormAttachment(1000, 1000, 0);
				messagesTextLData.bottom =  new FormAttachment(1000, 1000, -35);
				messagesText = new Text(dialogShell, SWT.MULTI | SWT.READ_ONLY | SWT.V_SCROLL);
				messagesText.setLayoutData(messagesTextLData);
			}
			dialogShell.setLocation(getParent().toDisplay(100, 100));
			dialogShell.open();
			dialogShell.addListener(SWT.Close, new Listener() {
		      public void handleEvent(Event event) {
		    	  if(! allowClose) {
		    		  event.doit = false;
		    		  appendLine("Vesta is still running ...");
		    	  }
		      }
		    });
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public void appendLine(String line){
		if(messagesText.getText().equals(""))
			messagesText.setText(line);
		else
			messagesText.insert(messagesText.getLineDelimiter() + line);
		messagesText.setSelection(messagesText.getText().length());
	}
	
	public void setAllowClose(boolean allow){
		this.allowClose = allow;
	}
	
	
}

package workbenchhelpers;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.swt.internal.win32.OS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * A simple HTTP server that uses NanoHTTPD to receive commands from
 * Mathematica.
 * 
 * @author Daniel
 */
public class HttpServer extends NanoHTTPD
{
    private static IWorkbenchWindow window;
    
    private String additionalOutput;

	public HttpServer()
    {
        super(8193);
    }

    @Override public Response serve(String uri, Method method, Map<String, String> header, Map<String, String> parms, Map<String, String> files)
    {
    	String response = "OK";
    	
    	String commandRes = null;
    	setAdditionalOutput("");
    	
    	try
    	{
	    	if (uri.equals("/go"))
	    	{
	            String command = parms.get("command");
	            if (command != null)
	            {
	            	command = command.trim();
	            
		            if (command.equals("open"))
		            {
		            	commandRes = openFileCommand(parms);
		            }
		            else if (command.equals("version"))
		            {
		            	response = "1.0.0.6";
		            }
		            else if (command.equals("selectFile"))
		            {
		            	commandRes = selectFileCommand(parms);
		            }
		            else if (command.equals("selectFolder"))
		            {
		            	commandRes = selectFolderCommand(parms);
		            }
		            else if (command.equals("test1"))
		            {
		            	// Do nothing. Just a round trip test.
		            }
		            else if (command.equals("test2"))
		            {
		            	String fileParam = parms.get("file");
		            	if (fileParam.equals("/Alpha/Source/CalculateParse/JavaTokenizer.m"))
		            	{
		            		response = "OK";
		            	}
		            	else
		            	{
		            		response = "FAILED";
		            	}
		            }
		            else if (command.equals("test3"))
		            {
		            	String fileParam = parms.get("file");
		            	final IFile file = getFile(fileParam);
		            	if (file != null)
		            	{
		            		response = "OK";
		            	}
		            	else
		            	{
		            		response = "FAILED";
		            	}
		            }
		            else if (command.equals("test4"))
		            {
		                final IWorkbenchWindow windowToUse = determineWhichWindowToUse();
		            	if (windowToUse != null)
		            	{
		            		response = "OK";
		            	}
		            	else
		            	{
		            		response = "FAILED";
		            	}
		            }
		            else if (command.equals("test5"))
		            {
		            	final IWorkbenchWindow windowToUse = determineWhichWindowToUse();
		            	final IWorkbenchPage page = windowToUse.getActivePage();
		            	if (page != null)
		            	{
		            		response = "OK";
		            	}
		            	else
		            	{
		            		response = "FAILED";
		            	}
		            }
		            else if (command.equals("test6"))
		            {
		            	final IWorkbenchWindow windowToUse = determineWhichWindowToUse();
		                Display.getDefault().syncExec(new Runnable() {
		                    @Override
		                    public void run()
		                    { 	        	
	            	        	bringToFront(windowToUse);
		                    }
		                });
		            }
		            else if (command.equals("test7"))
		            {
		            	String fileParam = parms.get("file");
		            	final IFile file = getFile(fileParam);
		            	final IWorkbenchWindow windowToUse = determineWhichWindowToUse();
		            	final IWorkbenchPage page = windowToUse.getActivePage();
		                Display.getDefault().syncExec(new Runnable() {
		                    @Override
		                    public void run()
		                    { 	        	
		                    	try
		                    	{
									IEditorPart part = IDE.openEditor(page, file, true);
								}
		                    	catch (PartInitException e)
		                    	{
		        	        		String msg = "Failed to open file: " + e.getMessage() + "\n\n" + getStackTrace(e);
		        	        		addOutput("test7: " + msg);
								}
		                    }
		                });
		            }
	    		}
	    	}
    	}
    	catch (final Exception e)
    	{
    		response = "WorkbenchHelper exception: " + e.getMessage() + "\n\n" + getStackTrace(e);
    		final String exceptionMessage = response;
    		
            Display.getDefault().syncExec(new Runnable() {
                @Override
                public void run() {
    				System.out.println("WorkbenchHelper exception: " + e.getMessage());
    				e.printStackTrace();
    				
    				//Throwable.printStackTrace(java.io.PrintWriter)
    				
    				MessageDialog.openInformation(
    					window.getShell(),
    					"WorkbenchHelpers (Code A2)",
    					exceptionMessage
    				);
                }
            });
    	}
    	
    	if (commandRes != null)
    	{
    		response = commandRes;
    	}
    	
    	if (!getAdditionalOutput().equals(""))
    	{
    		response = "FAILED\n\n";
    		response += getAdditionalOutput();
    	}
    	
    	System.out.println(response);
        
        return new NanoHTTPD.Response(response);
    }
    
    protected synchronized String getAdditionalOutput()
    {
    	return additionalOutput;
    }
    
    protected synchronized void setAdditionalOutput(String value)
    {
    	additionalOutput = value;
    }
    
    /**
     * Selects a file in the package explorer.
     * 
     * To select a folder, use: selectFolderCommand
     */
    private String selectFileCommand(Map<String, String> parms)
    {
        String fileParam = parms.get("file");
        if (fileParam != null) { fileParam = fileParam.trim(); }
        
        final IFile file = getFile(fileParam);
        
        if (file == null)
        {
        	return "File not found in Workbench: " + fileParam;
        }
        
        final IWorkbenchWindow windowToUse = determineWhichWindowToUse();
        final IWorkbenchPage page = windowToUse.getActivePage();
        
        Display.getDefault().syncExec(new Runnable() {
            @Override
            public void run() {
	        	// Bring the workbench window to the front.
	        	bringToFront(windowToUse);
	        	
	        	// Select the file in the package explorer.
	        	// Is this desirable when opening files?
	        	PackageExplorerPart packageExplorer = PackageExplorerPart.getFromActivePerspective();
	        	if (packageExplorer != null)
	        	{
	        		packageExplorer.selectAndReveal(file);
	        	}
	        	else
	        	{
	        		String msg = "Couldn't find Package Explorer.";
	        		addOutput(msg);
	        		System.out.println(msg);
	        	}
            }
        }
        );
        
        return null;
    }
    
    /**
     * Can be used to accumulate output to send back to the client, such as exception details.
     */
    protected synchronized void addOutput(String msg)
    {
		if (!additionalOutput.equals(""))
		{
			additionalOutput += "\n\n";
		}
		
		System.out.println("Add output: " + msg);
		
		additionalOutput += msg;
	}

	/**
     * Selects a folder in the package explorer.
     */
    private String selectFolderCommand(Map<String, String> parms)
    {
        String folderParam = parms.get("folder");
        if (folderParam != null) { folderParam = folderParam.trim(); }
        
        final IFolder folder = getFolder(folderParam);
        
        if (folder == null)
        {
        	return "Folder not found in Workbench: " + folderParam;
        }   
        
        final IWorkbenchWindow windowToUse = determineWhichWindowToUse();
        final IWorkbenchPage page = windowToUse.getActivePage();
        
        Display.getDefault().syncExec(new Runnable() {
            @Override
            public void run() {
            	try
            	{
		        	// Bring the workbench window to the front.
		        	bringToFront(windowToUse);
		        	
		        	// Select the folder in the package explorer.
		        	// Is this desirable when opening folders?
		        	PackageExplorerPart packageExplorer = PackageExplorerPart.getFromActivePerspective();
		        	if (packageExplorer != null)
		        	{
		        		packageExplorer.selectAndReveal(folder);
		        	}
		        	else
		        	{
		        		String msg = "Couldn't find Package Explorer.";
		        		addOutput(msg);
		        		System.out.println(msg);
		        	}
            	}
    	        catch (Exception e)
    	        {
    				System.out.println("Failed to select folder: " + e.getMessage());
    				e.printStackTrace();
    				
	        		String msg = "Failed to select folder: " + e.getMessage() + "\n\n" + getStackTrace(e);
	        		addOutput("A3: " + msg);
    				
    				//Throwable.printStackTrace(java.io.PrintWriter)
    				
    				MessageDialog.openInformation(
    					window.getShell(),
    					"WorkbenchHelpers (Code A3)",
    					msg
    				);
    			}
            }
        }
        );
        
        return null;
    }
    
    /**
     * Given the file parameter, returns the IFile. Refreshes the workspace if the file isn't yet found.
     * 
     * @param fileParam		the file parameter we were given.
     */
    private IFile getFile(String fileParam)
    {
    	IPath path = new Path(fileParam);
        final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
        
        // If the file doesn't exist, it might have just been created. Refresh.
        // But actually, we're going to do this regardless now, because we
        // need it for cases where we've just modified the file we're about to
        // open.
        //if (file != null && !file.exists())
        if (true)
        {
        	try
        	{
				file.refreshLocal(IResource.DEPTH_ONE, null);
			}
        	catch (CoreException e)
        	{
        		String msg = "Exception during refreshLocal: " + e.getMessage() + "\n\n" + getStackTrace(e);
        		addOutput(msg);
				System.out.println(msg);
			}
        }
        
        return file;
    }
    
    /**
     * Given the folder parameter, returns the IFolder. Refreshes the workspace if the folder isn't yet found.
     * 
     * @param folderParam		the folder parameter we were given.
     */
    private IFolder getFolder(String folderParam)
    {
    	IPath path = new Path(folderParam);
        final IFolder folder = ResourcesPlugin.getWorkspace().getRoot().getFolder(path);     
        
        // If the folder doesn't exist, it might have just been created. Refresh.
        if (folder != null && !folder.exists())
        {
        	try
        	{
				folder.refreshLocal(IResource.DEPTH_ONE, null);
			}
        	catch (CoreException e)
        	{
        		String msg = "Exception during refreshLocal: " + e.getMessage() + "\n\n" + getStackTrace(e);
        		addOutput(msg);
			}
        }
        
        return folder;
    }
    
    /**
     * Bring the workbench window to the front.
     */
    private void bringToFront(IWorkbenchWindow window)
    {
    	// Bring the workbench window to the front.
		// https://bugs.eclipse.org/bugs/show_bug.cgi?id=192036
    	Shell shell = window.getShell();
    	shell.forceActive();
    	shell.setMinimized(false);
    	if (System.getProperty("os.name").contains("Windows"))
    	{
    		forceActive(shell);
    	}
    	shell.setActive();
    }
    
    /**
     * Open a file in the editor, and possible go to a particular line.
     */
    private String openFileCommand(Map<String, String> parms)
    {
        String fileParam = parms.get("file");
        if (fileParam != null) { fileParam = fileParam.trim(); }
        
        final String line = parms.get("line");
        
        final IFile file = getFile(fileParam);
        
        System.out.println("Open: " + fileParam);
        System.out.println("  IFile: " + file);
        
        if (file == null)
        {
        	return "File not found in Workbench: " + fileParam;
        }
        
        final IWorkbenchWindow windowToUse = determineWhichWindowToUse();
        final IWorkbenchPage page = windowToUse.getActivePage();
        
        Display.getDefault().syncExec(new Runnable() {
            @Override
            public void run() {
    	        try
    	        {    	        	
		        	// Bring the workbench window to the front.
    	        	bringToFront(windowToUse);
    	        	
    	        	IEditorPart part = IDE.openEditor(page, file, true);
    	        	
    	        	// Select the file in the package explorer.
    	        	// Is this desirable when opening files?
    	        	PackageExplorerPart packageExplorer = PackageExplorerPart.getFromActivePerspective();
    	        	if (packageExplorer != null)
    	        	{
    	        		packageExplorer.selectAndReveal(file);
    	        	}
    	        	else
    	        	{
    	        		String msg = "Couldn't find Package Explorer.";
    	        		addOutput(msg);
    	        		System.out.println(msg);
    	        	}

    	        	if (line != null)
    	        	{
    	        		int lineNum = Integer.parseInt(line.trim());
    	        		
    					if (part instanceof ITextEditor)
    					{	
	    					ITextEditor editor = (ITextEditor) part;
	    					
	    					IRegion region = getLineInformation(editor, lineNum - 1);
	    					
	    					if (region != null)
	    					{
	    						editor.selectAndReveal(region.getOffset(), 0);
	    					}
    					}
    	        	}
    			}
    	        catch (Exception e)
    	        {
    				System.out.println("Failed to open file: " + e.getMessage());
    				e.printStackTrace();
    				
            		String msg = "Failed to open file: " + e.getMessage() + "\n\n" + getStackTrace(e);
            		addOutput("A1: " + msg);
    				
    				MessageDialog.openInformation(
    					window.getShell(),
    					"WorkbenchHelpers (Code A1)",
    					msg
    				);
    			}
            }
        });
        
        return null;
    }
    
    /**
     * Converts an exception's stack trace to a string.
     */
    public static String getStackTrace(Throwable aThrowable)
    {
        Writer result = new StringWriter();
        PrintWriter printWriter = new PrintWriter(result);
        aThrowable.printStackTrace(printWriter);
        return result.toString();
    }
    
    /**
     * Returns the window that we should use. We favor the window containing
     * the "Mathematica Development" perspective, if we can find one.
     */
    IWorkbenchWindow determineWhichWindowToUse()
    {
        IWorkbenchWindow mathematicaDevelopmentWindow = getWolframWorkbenchWindow();
        IWorkbenchWindow windowToUse = null;
        
        if (mathematicaDevelopmentWindow != null)
        {
        	windowToUse = mathematicaDevelopmentWindow;
        }
        else
        {
        	// Fall back to using the first window.
        	
            IWorkbench wb = PlatformUI.getWorkbench();
            IWorkbenchWindow[] windows = wb.getWorkbenchWindows();
        	
            if (windows.length >= 0)
            {
            	windowToUse = windows[0];
            }
            else
            {
            	// If we can't seem to find any windows, use the
            	// one that was there when the plugin initialized.
            	windowToUse = window;
            }
        }
        
        return windowToUse;
    }
    
    /**
     * Returns the Workbench window that is in the "Mathematica Development"
     * perspective. If not found, returns null.
     */
    IWorkbenchWindow getWolframWorkbenchWindow()
    {
        IWorkbench wb = PlatformUI.getWorkbench();
        IWorkbenchWindow[] windows = wb.getWorkbenchWindows();
        
        for (IWorkbenchWindow window : windows)
        {
	        IWorkbenchPage page = window.getActivePage();
	        IPerspectiveDescriptor perspective = page.getPerspective();
	        String label = perspective.getLabel();
	        
	        if (label.equals("Mathematica Development"))
	        {
	        	return window;
	        }
        }
        
        return null;
    }
    
    // https://bugs.eclipse.org/bugs/show_bug.cgi?id=192036
    public void forceActive(Shell shell)
    {
    	int hFrom = (int)OS.GetForegroundWindow();

        if (hFrom <= 0) {
          OS.SetForegroundWindow(shell.handle);
          return;
        }

        if (shell.handle == hFrom) {
          return;
        }

        int pid = OS.GetWindowThreadProcessId((int)hFrom, null);
        
        int _threadid = OS.GetWindowThreadProcessId(shell.handle, null);

        if (_threadid == pid) {
          OS.SetForegroundWindow(shell.handle);
          return;
        }

        if (pid > 0) {
          if ( !OS.AttachThreadInput(_threadid, pid, true)) {
            return;
          }
          OS.SetForegroundWindow(shell.handle);
          OS.AttachThreadInput(_threadid, pid, false);
        }

        OS.BringWindowToTop(shell.handle);
        OS.UpdateWindow(shell.handle);
        OS.SetActiveWindow(shell.handle);
    }
    
    // Stolen from Workbench plugin.
	private static IRegion getLineInformation(ITextEditor editor, int lineNumber) {
		IDocumentProvider provider = editor.getDocumentProvider();
		IEditorInput input = editor.getEditorInput();
		try {
			provider.connect(input);
		} catch (CoreException e) {
			return null;
		}
		try {
			IDocument document = provider.getDocument(input);
			if (document != null)
				return document.getLineInformation(lineNumber);
		} catch (BadLocationException e) {
		} finally {
			provider.disconnect(input);
		}
		return null;
	}

	public static void setWindow(IWorkbenchWindow window)
	{
		HttpServer.window = window;
	}
}
use Tk::ColorEditor;
$mw = MainWindow->new();
$cref = $mw->ColorEditor();
$cref->configure(-bg=>'white');
$cref->Show;

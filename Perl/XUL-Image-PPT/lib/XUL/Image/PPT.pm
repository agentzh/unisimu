package XUL::Image::PPT;

use 5.006001;
use Moose;

use File::Spec;
use Win32::OLE;
use Win32::OLE::Const;

has 'from'  => (is => 'rw', isa => 'Int', default => 1);
has 'indir' => (is => 'rw', isa => 'Str', default => 'xul_img');

sub go {
    my $self = shift;
    my $app = Win32::OLE->GetActiveObject("PowerPoint.Application");
    my $show;
    if (!$app) {
        $app = Win32::OLE->new('PowerPoint.Application')
            or die Win32::OLE->LastError;
        $app->{Visible} = 1;
        $app->Presentations->Add;
    }
    $show = $app->ActivePresentation;
    if (!$show) {
        $app->Presentations->Add;
        $show = $app->ActivePresentation;
    }        
    
    my $const = Win32::OLE::Const->Load($app);
    my $slides = $show->Slides;

    my $listing = File::Spec->catfile($self->indir, 'listing.txt');
    open my $in, $listing
        or die "Cannot open $listing for reading: $!\n";
    my $i = $self->from;
    while (<$in>) {
        chomp;
        next if /^\s*$/;
        my $fbase = $_;
        my $fname = File::Spec->catfile($self->indir, $fbase);
        my $slide = $slides->Add($i++, $const->{ppLayoutBlank});
        warn "inserting $fname...\n";
        my $msoFalse = 0;
        my $msoTrue  = -1;
        my $pic = $slide->Shapes->AddPicture(
            File::Spec->rel2abs($fname),   # FileName
            $msoFalse,                     # LinkToFile
            $msoTrue,                      # SaveWithDocument
            0, 0,                          # Left and Top
        ) or die "error: Failed to insert picture $fname.\n";
        #$pic->Scaleheight(1, $msoTrue);
        #$pic->Scalewidth (1, $msoTrue);
    }
}

1;
__END__

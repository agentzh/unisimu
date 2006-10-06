package XUL::Image::PPT;

use 5.006001;
use Moose;

use File::Spec;
use Win32::OLE;
use Win32::OLE::Const;

has 'from'  => (is => 'ro', isa => 'Int', default => 1);
has 'indir' => (is => 'ro', isa => 'Str', default => 'xul_img');

sub go {
    my $self = shift;
    my $app = Win32::OLE->GetActiveObject("PowerPoint.Application");
    my $show;
    if (!$app) {
        $app = Win32::OLE->new('PowerPoint.Application')
            or die Win32::OLE->LastError;
        $app->{Visible} = 1;
        $app->Presentations->Add;
        $show = $app->ActivePresentation;
    } else {
        $show = $app->ActivePresentation;
        if (!$show) {
            $app->Presentations->Add;
            $show = $app->ActivePresentation;
        }        
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
        $slide->Shapes->AddPicture(File::Spec->rel2abs($fname), 0, -1, -23, -5);
    }
}

1;
__END__

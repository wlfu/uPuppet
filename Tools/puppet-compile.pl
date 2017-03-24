#!/usr/bin/perl

=begin comment

puppet-compile

This script calls the Puppet compiler to compile example Puppet scripts
and present the output in a readable form. The "-v" option makes it
more verbose.

INITIALISATION
The script creates a per-user copy of the necessary Puppet files in
~/.puppet-compile. Before using the script for the first time, use
"puppet-compile -iv" to initialise this working directory. You can
delete the directory & initialise again at any time. Use the "-d"
option to specify a different working directory.

BASICS
If you have a Puppet script foo.pp, you can compile it with
"puppet-compile foo.pp". A sanitised version of the resulting catalog
will be written to stdout (in JSON format). Use "-l" to get the full
version. Use "-o" to set the options on the puppet compiler. The "-s"
option provides more control over the output elements.

NODE STATEMENTS
If the source file does not contain a "node" statement, a default one
will be added which includes the class "main". This means that source
files need only contain a "main" class. The "-n" and "-c" options can
be used to change the name and class of the default node statement.

CLASSES
By default, all of the source file is copied into the main site.pp
file. If you include an annotation of the following form in the source
file, all of the following code will be copied into the corresponding
module directory instead: # @class: CLASSNAME (you will probably never
need to do this).

LATEX OUTPUT
The script can produce latex output suitable for direct inclusion in
a latex source file. Use "-t SPEC", where spec include one or more of ...
- 1 = single column style
- 2 = double column style
- f = enclose in figure
- m = enclose in minipage
- b = retain blank lines (normally removed)
The output requires lstlisting and the apperance can be customised using the
lstlisting options.

LATEX ANNOTATIONS
The source file can include the following annotations:
- # @caption: CAPTION - define caption for figure
- # @label: LABEL - define label for figure
= # % LATEX SOURCE = include latex source above listing (multiple lines allowed)
- # \... - include arbitrary latex commands
Arbitrary latex commands appear in the output enclosed in "^"
characters. This should be defined as the escape character in 
the lstlisting options (escapechar=^)

Paul Anderson <dcspaul@ed.ac.uk>

=cut

use strict;
use Getopt::Std;
use File::Basename;
use Text::Tabs;

my $prog = "puppet-compile";			# program name
my $home = $ENV{'HOME'};				# home dir

my $has_node = 0;						# source files include a node statement
my $verbose = 1;						# verbosity level (0-2)
my $data_dir = undef;					# data directory
my $module_dir = undef;					# module directory
my $sitepp = undef;						# site.pp
my $master_module_dir = undef;			# master module directory
my $master_sitepp = undef;				# master site.pp
my $tool_dir = undef;					# source tools directory
my $environment = undef;				# environment
my $node = undef;						# node
my $class = undef;						# class
my $data_dir = undef;					# data directory
my $output_dir = undef;					# output directory
my $puppet_options = undef;				# puppet options
my $long = 0;							# long json format
my $tmpfile = undef;					# temporary file
my $types = undef;						# types to display
my $latex_opts = undef;					# latex output options
my $init = 0;							# initialise directories & modules
my @sources = ();						# source files
my $source_name = undef;				# name of source for Latex etc

my $tabsize = 2;						# latex tab size
my $tex_escape = '^';					# escape listing mode
my $tex_blanklines = undef;				# true to retain blank lines
my $tex_figure = undef;					# true to make latex figure
my $tex_minipage = undef;				# true to make latex minipage
my $tex_format = undef;					# format style
my $tex_caption = undef;				# latex caption
my $tex_label = undef;					# latex label
my $tex_expecterr = undef;				# expect error for latex
my $tex_annotations = undef;			# show annotations

#########################################################################################
sub Usage() {
##########################################################################################

	print stderr <<EOF;
usage: $prog [opts] file ...
 -c class         - default class ($class)
 -d dir           - dst data directory ($data_dir)
 -e env           - environment ($environment)
 -i               - initialise directories & modules (no)
 -l               - long json output (no)
 -m dir           - dst module directory ($module_dir)
 -n node          - node ($node)
 -N name          - name for Latex etc ($source_name)
 -o options       - puppet options ($puppet_options)
 -q               - quiet (no)
 -S path          - dst site.pp ($sitepp)
 -s types         - comma-separate list of types to output ($types)
 -t opts          - latex output (no)
 -T dir           - tool directory ($tool_dir)
 -v               - verbose (no)
 -V               - very verbose (no)
EOF
	exit(1);
}

##########################################################################################
sub GetArguments() {
##########################################################################################

	my $opts = {};
	my $opt_string = 'c:d:e:gG:ilm:n:N:o:qrR:s:S:t:T:vV';
	my $single_opts = $opt_string; $single_opts =~ s/.://g;

	if (getopts( $opt_string, $opts )) {
		$class = $opts->{'c'};
		$data_dir = $opts->{'d'};
		$environment = $opts->{'e'};
		$init=1 if ($opts->{'i'});
		$long=1 if ($opts->{'l'});
		$module_dir = $opts->{'m'};
		$node = $opts->{'n'};
		$source_name = $opts->{'N'};
		$puppet_options = $opts->{'o'};
		$verbose=0 if ($opts->{'q'});
		$sitepp = $opts->{'S'};
		$types = $opts->{'s'};
		$latex_opts = $opts->{'t'};
		$tool_dir = $opts->{'T'};
		$verbose=2 if ($opts->{'v'});
		$verbose=3 if ($opts->{'V'});
	} else { $opts = undef; }

	# sources
	@sources = @ARGV;

	# defaults
	$data_dir = "$home/.puppet-compile" unless (defined($data_dir));
	$environment = "production" unless (defined($environment));
	$sitepp = "$data_dir/environments/$environment/manifests/site.pp" unless (defined($sitepp));
	$module_dir = "$data_dir/environments/$environment/modules" unless (defined($module_dir));
	$tool_dir = "/usr/share/puppet-tools" unless (defined($tool_dir));
	$master_module_dir = "$tool_dir/environments/$environment/modules";
	$master_sitepp = "$tool_dir/environments/$environment/manifests/site.pp";
	$node = "default" unless (defined($node));
	$class = "main" unless (defined($class));
	$output_dir = "$data_dir/output";
	$puppet_options = "" unless (defined($puppet_options));
	$tmpfile = "$output_dir/$node.tmp";
	$types = "User,File,Package" unless (defined($types));

	# latex format
	$tabstop = $tabsize;
	my $caption; if ($source_name) {
		$caption = $source_name;
	} else {
		$caption = join ',',@sources; $caption =~ s/.*\///;
	}
	my $label; if ($source_name) {
		$label = $source_name;
		$label =~ s/[^0-9a-zA-z-]/-/g;
	} else {
		$label = @sources[0]; $label =~ s/.*\///; $label =~ s/\..*//;
	}
	$tex_figure = ($latex_opts =~ /f/) ? 1 : 0;
	$tex_minipage = ($latex_opts =~ /m/) ? 1 : 0;
	$tex_blanklines = ($latex_opts =~ /b/) ? 1 : 0;
	$tex_format = ($latex_opts =~ /2/) ? 2 : 1;
	$tex_caption = ($caption) ? $caption : undef;
	$tex_label = ($label) ? "exfig:$label" : undef;
	$tex_expecterr = ($latex_opts =~ /e/) ? 1 : 0;
	$tex_annotations = ($latex_opts =~ /a/) ? 1 : 0;

	Usage() unless (defined($opts) && ($init || ((scalar @sources) > 0)));
}

##########################################################################################
sub ReadFiles() {
##########################################################################################

	my $files = {};
	my $src = "";
	my $comment = "";

	# we create a default site.pp file which starts with the canned pre-amble 
	# from the site.pp (if any) which we ship as default the give environment
	my $f = $master_sitepp;
	if (-f $f) {
		print stderr "master site.pp: $f\n" if ($verbose>1);
		my $out_file = "$sitepp"; $files->{$out_file} = "";
		unless (open(FP,"<$f")) { print stderr "$prog: $f\n- \l$!\n"; exit 1; }
		while (<FP>){
			my $line = $_;
			$files->{$out_file} .= $line;
		}
		close(FP);
	} else {
		print stderr "master site.pp: none\n" if ($verbose>1);
	}

	# source files
	foreach my $f (@sources) {
		print stderr "reading: $f\n" if ($verbose>1);
		my $out_file = "$sitepp";
		unless (open(FP,"<$f")) { print stderr "$prog: $f\n- \l$!\n"; exit 1; }
		while (<FP>){
			my $line = $_;
			# if the source contains a node statement
			# we don't want to generate one automatically
			$has_node=$line if ($line =~ /^\s*node\s+/);
			# lines of this form are used to put the following code
			# into separate class files (the lines themselves are deleted)
			# # @class: foo
			# # @class: foo::bar
			if ($line =~ /^\s*\#\s*\@class:\s+([a-zA-Z0-9_]+)(::([a-zA-Z0-9_]+))?\s*$/ ) {
				my $class = $1;
				my $subclass = $3 || "init";
				$out_file = "$module_dir/$class/manifests/$subclass.pp";
			# lines of this form can be used to set the figure label
			# # @label: label
			} elsif ($line =~ /^\s*\#\s*\@label:\s+(.*\S)\s*$/ ) {
				$tex_label = $1;
			# lines of this form can be used to set the figure caption
			# # @caption: some caption
			} elsif ($line =~ /^\s*\#\s*\@caption:\s+(.*\S)\s*$/ ) {
				$tex_caption = $1;
			# other annotations are removed unless explicitly required
			} elsif (!$tex_annotations && ($line =~ /^\s*\#\s*\@/) ) {
				$line =~ s/\s*\#\s*\\.*//;
			# lines of this form are raw latex to include before the figure
			} elsif ($line =~ /\s*\#\s*\%\s*(.*)$/) {
				$comment .= "$1\n";
			} else {
				# comments of the form # \stuff are assumed to be embedded latex
				# they are removed from the code to be compiled ($line)
				# and the # removed from the tex file ($latex_line)
				my $latex_line = $line;
				$line =~ s/\s*\#\s*\\.*//;
				if (defined($files->{$out_file})) { $files->{$out_file} .= $line;  }
				else { $files->{$out_file} = $line; }
				$latex_line =~ s/\s*\#\s*(\\.*)/$tex_escape\1$tex_escape/;
				($latex_line) = expand($latex_line);
				$src .= $latex_line if ($tex_blanklines || ($latex_line !~ /^\s*\n?$/));
			}
		}
		close(FP);
	}

	# if the sources files do not include a "node" statement, add one
	if (!$has_node) {
		print stderr "adding node statement: node $node { include $class }\n"
			if ($verbose>1);
		$files->{$sitepp} .= <<EOF
node $node { include $class }

EOF
	} else {
		print stderr "source includes node: $has_node" if ($verbose>1);
	}

	return ($files,$src,$comment);
}

##########################################################################################
sub WriteFiles($) {
##########################################################################################

	my $files = shift;
	foreach my $f (keys %$files) {
		my $dir = dirname($f);
		my $result = `mkdir -p $dir 2>&1`;
		unless ($?==0) { print stderr "$prog: $dir\n- \l$!\n"; exit 1; }
		print stderr "writing: $f\n" if ($verbose>1);
		unless (open(FP,">$f")) { print stderr "$prog: $f\n- \l$!\n"; exit 1; }
		print FP $files->{$f};
		close(FP);
	}
}

##########################################################################################
sub Initialise() {
##########################################################################################

	# create directories
	foreach my $d ( "environments", "output" ) {
		my $dir = "$data_dir/$d";
		print stderr "creating: $dir\n" if ($verbose>1);
		my $res = `mkdir -p $dir 2>&1`;
		unless ($?==0) { print stderr "$prog: $dir\n- \l$res\n"; exit 1; }
	}

	# find modules
	my @modules = ();
	if (-d $master_module_dir) {
		if (opendir DP, $master_module_dir) {
			print stderr "$prog: $master_module_dir\n- \l$!\n"; exit 1;
		}
		@modules = readdir DP;
		closedir(DP);
	}

	# create links
	foreach my $m (@modules) {
		if ( !($m=~/^\./) && -d "$master_module_dir/$m") {
			my $l = "$module_dir/$m";
			if (! -l $l) {
				if (-e $l) {
					print stderr "$prog: warning: standard lib overridden: $l\n";
				} else {
					print stderr "module: $m\n" if ($verbose>1);
					my $result = `mkdir -p $module_dir 2>&1`;
					unless ($?==0) { print stderr "$prog: mkdir -p $module_dir\n- \l$result\n"; exit 1; }
					unless (symlink("$master_module_dir/$m",$l)) {
						 print stderr "$prog: link $master_module_dir/$m $l\n- \l$!\n"; exit 1; 
					}
				}
			}
		}
	}
}

##########################################################################################
sub Compile() {
##########################################################################################

	my $result = `mkdir -p $output_dir 2>&1`;
	unless ($?==0) { print stderr "$prog: mkdir -p $output_dir\n- \l$result\n"; exit 1; }

	my $cmd = "puppet master $puppet_options"
		. " --environment $environment"
		. " --environmentpath $data_dir/environments"
		. " --compile $node";

	print stderr "compile: $cmd\n" if ($verbose>1);
	$result = `$cmd 2>&1 >$tmpfile`;
	unless ($?==0) {
		$result =~ s/\e\[[0-9;]*m//g; # remove display control sequences ?
		# if we have a compilation error in Latex mode, then try
		# to return a sensible summary of the error for display
		# otherwise, fail ....
 		if (defined($latex_opts) && $tex_expecterr) {
 			my $msg = MakeLatexError($result);
 			if (defined($msg)) { return (1,$msg) }
 			else { return "compilation failed"; }
 		} 
		print stderr "$prog: compilation failed\n$result\n$cmd";
		unlink($tmpfile);
		exit 1;
	}

	my $output = "";
	unless (open(FP,"<$tmpfile")) {
		print stderr "$prog: $tmpfile\n- \l$!\n"; exit 1;
	}
	while (<FP>){
		my $line = $_;
		print stderr "==> $line" if ($verbose>1);
		$output .= $line if ($line =~ /^[ \t{}]/);
	}
	close(FP);
	unlink($tmpfile);

	return (0,$output);
}

##########################################################################################
sub TrimOutput($) {
##########################################################################################

# use jq to extract the relevant stuff from the catalog

	my $output = shift;

	# write file
	unless (open(FP,">$tmpfile")) { print stderr "$prog: $tmpfile\n- \l$!\n"; exit 1; }
	print FP $output;
	close(FP);

	# filter it-  jq manual here: http://stedolan.github.io/jq/manual/
	my $query = "";
	foreach my $t (split /\s*,\s*/,$types) {
		$query .= " or .type == \"$t\"";
	} $query =~ s/^\s*or\s*//;

	my $filter = <<EOF;
.resources |
map(select($query)) |
map(del(.line)) |
map(del(.file)) |
map(del(.tags)) |
map(del(.exported)) 
EOF
# | map(del(.parameters.ensure))
	my $result = `jq '$filter' <$tmpfile >$tmpfile#`;
	unless ($?==0) {
		print stderr "$prog: jq failed\n$result";
		unlink($tmpfile);
		unlink("$tmpfile#");
		exit 1;
	}
	unlink($tmpfile);

	# load the result
	$output = "";
	unless (open(FP,"<$tmpfile#")) {
		print stderr "$prog: $tmpfile#\n- \l$!\n"; exit 1;
	}

	while (<FP>){
		my $line = $_;
		next if $line =~ /^\[\s*$/;
		next if $line =~ /^\]\s*$/;
		$line =~ s/  //;
		$output .= $line;
	}
	close(FP);
	unlink("$tmpfile#");

	return $output;
}

##########################################################################################
sub MakeLatex($$$) {
##########################################################################################

	my $src = shift;
	my $output = shift;
	my $comment = shift;

	my $tex_placement = ($tex_format==1) ? "H" : "!htb";
	my $tex_w1 = ($tex_format==1) ? "\\textwidth" : ".55\\linewidth";
	my $tex_w2 = ($tex_format==1) ? "\\textwidth" : ".40\\linewidth";

	my @files = (); foreach my $p (@sources) {
		my $f = $p; $f =~ s/.*\///;
		push @files,($f);
	}
	my $files; if ($source_name) {
		$files = $source_name;
	} else {
		$files = join ',',@files;
	}
	if ($files eq "-") { $files=""; } else { $files=" from $files"; }

	my $tex = "";
	$tex .=  "% ** DO NOT EDIT ** - generated$files by $prog\n";
	$tex .= "\\begin{figure}[$tex_placement]\n" if ($tex_figure);
	$tex .= "\\begin{minipage}{\\textwidth}\n" if ($tex_minipage && $tex_format==1);
	$tex .= "\\begin{minipage}{.55\\linewidth}\n" if ($tex_minipage && $tex_format==2);
	$tex .= "$comment\\vspace{1ex}\n" if ($comment);
	$tex .= "\\begin{lstlisting}\n";
	$tex .= $src;
	$tex .= "\\end{lstlisting}\n";
	$tex .= "\\end{minipage}\n\\hfill\n" if ($tex_minipage && $tex_format==2);
	$tex .= "\\begin{minipage}{.40\\linewidth}\n" if ($tex_minipage && $tex_format==2);
	$tex .= "\\begin{lstlisting}\n";
	$tex .= $output;
	$tex .= "\\end{lstlisting}\n";
	$tex .= "\\end{minipage}\n" if ($tex_minipage);
	$tex .= "\\caption{$tex_caption}\n" if (defined($tex_caption) && $tex_figure);
	$tex .= "\\label{$tex_label}\n" if (defined($tex_label) && $tex_figure);
	$tex .= "\\end{figure}\n" if ($tex_figure);

	return $tex;
}

##########################################################################################
sub MakeLatexError($) {
##########################################################################################

# try to make a really short description of the error
# for use in latex examples

	my $msg = shift;

	foreach my $line (split "\n",$msg) {
		next unless ($line =~ /Error:\s*(.*)$/);
		my $s = $1;
		$s =~ s/\..*//;
		$s =~ s/\:.*//;
		$s =~ s/ at .*//;
		$s =~ s/ by .*//;
		$s =~ s/ while .*//;
		return "Error:\n$s\n";
	}
	return undef;
}

##########################################################################################
sub DoAll() {
##########################################################################################

	Initialise() if ($init);
	if ((scalar @sources) > 0) {
		my ($files,$src,$comment) = ReadFiles();
		WriteFiles($files);
		my ($err,$output) = Compile();
		$output = TrimOutput($output) unless ($long || $err);
		if (defined($latex_opts)) { $output = MakeLatex($src,$output,$comment); }
		else {
			$output =~ s/\n*$//;
			$output =~ s/\n/\n  /g;
			$output = "\[\n  $output\n\]" unless ($output eq "[]");
			$output .= "\n";
		}
		return $output;
	} else { return ""; }
}

##########################################################################################
# main
##########################################################################################

GetArguments();

my $output = DoAll();

print $output;

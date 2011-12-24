#!/usr/bin/perl

#*********************************************************************
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available as
# '/usr/share/common-licences/GPL' in the Debian GNU/Linux distribution
# or on the World Wide Web at http://www.gnu.org/copyleft/gpl.html.  You
# can also obtain it by writing to the Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#*********************************************************************

use File::Find;
use File::stat;
use File::Copy;
use Getopt::Long;
Getopt::Long::Configure ('bundling');

$version = "0.1";

=head1 NAME

	fai-replace

=head1 SYNOPSIS

	Put this script in $FAI/bin so you get $FAI/bin/fai-replace
	Create CLASSNAME.rpl in $FAI/config/
	Add the installation client to this class.
	
	Setup perl hashes in the .rpl file:
	$FAI/config/FAIBASE.rpl:
		$FAIBASE{"systemname"} = "utopiaplanitita";
		$FAIBASE{"comment"} = "A comment";
		EOF

	$FAI/config/CLASSNAME.rpl:
		# special hash for importing variables from other classes
		$CLASSNAME{"requires"} = ("FAIBASE");

		# special hash array of filenames for replacment
		# usually is not necessary because they can be found automatically using the automode
		$CLASSNAME{"files"} = ("/etc/motd", "/etc/smb/samba.conf");
		
		$CLASSNAME{"version"} = "1.1";
		$CLASSNAME{"sysname_ver"} = $FAIBASE{"systemname"}.$CLASSNAME{"version"};
		EOF
	
	Use the expression [% CLASSNAME.version %] in any file in the following subdirectories of the FAI config space:
	/files /hooks /scripts , e.g.:

	$FAI/files/etc/motd/CLASSNAME:
		Welcome to [% systemname %] version [% CLASSNAME.version %]
		A little comment from FAIBAS [% FAIBASE.comment %]
		EOF

	If the CLASSNAME of template file can be derived from its path you can omit CLASSNAME an just write [% version %]

	Add a hook appropriate for each stage of string replacement:

	hooks/action.FAIBASE:
		$FAI/bin/fai-replace -m scripts
		$FAI/bin/fai-replace -m hooks
	
	hooks/savelog.FAIBASE:
		$FAI/bin/fai-replace -m files
	
	The fai hooks must be in the right stage of the installation, scripts and hook templates need to be replaced
	_before_ they are executed. File templates need to be replaced _after_ they were transfered to the install client

	SERVER MODE

	fai-replace supports a server mode where the same script can be run to template files on the FAI server.
	I use this to update my pammount.conf.xml file to make the users mount network shares according to the
	groups they belong to in the LDAP. The clients the rsync this pammount.conf.xml file regulary.
	To use this feature it is necassary to specify the server- command line options:

	Example:

	/srv/fai/bin/fai-replace -m server --server-config-directory=/srv/fai/config/config \
		--server-target-path=/home/getconfig/etc --server-post-scripts-path=/srv/fai/bin

	Server config directory is the directory with the *rpl files you use in FAI. If you use
	the nfs root you have to point it the, if you use SVN make a checkout before each run of fai-replace

	Server target-path is the path where the templates are. Templates have the same name as the real filename
	put are hidden and have the tmpl extension, example:
		/home/getconfig/etc/security/.pammount.conf.xml.tmpl
	
	Optionally you can run so-called post scripts on the template files. These are scripts that are run
	after the template replacing of fai-replace was done, they should accept a filename as input
	and leave a modified file with the same filename as given, example:

		/srv/fai/sbin/pammount.conf.xml.post

	For my example this script will get the LDAP data from the LDAP server and insert mount directives
	for pammount according to the users group memberships.

	The idea of this server mode is the possibitiy to have central configuration accross server
	and clients.

=head1 DESCRIPTION

 This scripts is an addition to FAI the
 "Fully Automatic Installation".
 It makes templating easier. And allows central
 configuration management.

=head1 AUTHOR


=cut

################# VARIABLES ##########################

# define true and false
my $true = 1;
my $false = 0;

#### COMMON

# extension of the files defining configuration files
my $replace_extension  = "rpl";
my $tmp_file_extension = "tmp";

my $tmp_files_are_hidden = $true;
my $keep_tmp_files       = $false;

# processing mode
my $pmode        = "";
my $check_regexp = "\[%.*[a-zA_Z0-9\.\_]*.*%\]";

# searches for template files belonging to
# the class and makes substitution instead
# of using the explicit definition of
# the files using a hash with the key
# $replace_array_name in the replacement files
my $automode = $false;

# turn debugging on
my $debug    = $true;

# error code
my $err      = $false;

#### SERVER MODE VARIABLES

my $run_by_server          = $false;
my $srv_config_dir         = "/srv/fai/tmp/config";
my $srv_file_path          = "/home/getconfig/config";
my $srv_tmpl_ext           = "tmpl";
my $srv_tmpl_are_hidden    = $true;
my $post_scripts_path      = "/srv/fai/sbin/";
my $post_scripts_extension = "post";
#### FAI MODE

# relative to config space the configuration files
# for fai-replace
$replace_dir = "config";

# name of the hash used to add files
# to the list of files to consider for
# template replacement when automode
# is not used
my $replace_array_name = "files";

# name of the has that contains the dependencies
# of this class
my $require_array_name = "require";

my $fai_files_dir  = "files";
my $fai_script_dir = "scripts";
my $fai_hooks_dir  = "hooks";

# name of the FAI envirnonment variable
# that contains the class naes
my $fai_classes_var = "classes";

# delimter in the $fai_classes_var that is
# used to split the string into class names
my $fai_classes_delimiter = "\\n";

# if template files exceed this size
# they are not considered for replacement
my $fai_max_tmpl_size = "300k";

# exclude templates matching these files extensions
my @fai_replace_exclude_extensions = ( "png", "jpg" );
################# ENVIRONMENT VARIABLES ##############
my $target         = "";
my $fai_action     = "";
my $faiconfigspace = "";
my $logfile        = "";
######################################################

################ START THE CODING ####################

my @fai_repalce_exclude_extensions_string;
my $help;
GetOptions (
	'server-config-directory:s' => \$srv_config_dir,
	'server-target-path:s' => \$srv_file_path,
	'server-post-scripts-path:s' => \$post_scripts_path,
	'max-template-size:s' => \$fai_max_tmpl_size,
	'exclude-extensions:s' => \$fai_replace_exclude_extensions_string,
	'v|verbose' => \$verbose,	
	'D|debug' => \$debug,
	'a|automode' => \$automode,
	'm|processing-mode=s' => \$pmode,
	'l|logfile:s' => \$logfile,
	's|fai-config-space:s' => \$faiconfigspace,
	'c|fai-classes:s' => \$classes,
	't|fai-target:s' => \$target,
	'h|help' => \$help
);

if($help){
	&usage();
	exit(0);
}

$verbose  = $verbose || $ENV{verbose} || $false;
$debug = $debug || $ENV{debug} || $false;
$automode = $automode || $false;
$logfile = $logfile ||  $ENV{LOGDIR} && "$ENV{LOGDIR}/replace-$pmode.log" || 0;
$faiconfigspace = $faiconfigspace || $ENV{FAI}  || `pwd`;
$target         = $target || $ENV{FAI_ROOT} || $ENV{target};

if(  $fai_replace_exclude_extensions_string){
	@fai_replace_exclude_extensions= split(',', $fai_replace_exclude_extensions_string);
}

$logfile
  and ( open( LOGFILE, ">> $logfile" ) || die("can't open logfile: $!") );

if (    $pmode ne "server"
    and $pmode ne "files"
    and $pmode ne "scripts"
    and $pmode ne "hooks" )
{
    &perr("No correct processing mode specified\n");
    &usage();
    &myexit(1);
}

&plog("Running in $pmode mode\n");
if ( &runByFai == 0 ) {
    &plog("Script is run by FAI client\n");
    if( $classes ){
	$fai_classes_delimiter= "\\.";
    }else{
	$classes = $ENV{$fai_classes_var};
    }

    $replace_dir    = $faiconfigspace . "/" . $replace_dir;
    $fai_files_dir  = $faiconfigspace . "/" . $fai_files_dir;
    $fai_script_dir = $faiconfigspace . "/" . $fai_script_dir;
    $fai_hooks_dir  = $faiconfigspace . "/" . $fai_hooks_dir;

    if ( $pmode eq "" ) {
        &perr("Script requires args: files, scriptsor hooks in FAI mode\n");
        &myexit(1);
    }
    elsif ( $pmode eq "scripts" ) { $automode = $true; }
    elsif ( $pmode eq "hooks" )   { $automode = $true; }

    &mainloopFAI;

}
elsif ( ( $run_by_server or $pmode eq "server" ) and not &runByFai == 0 ) {
    &plog("Script is run in Server mode\n");
    $replace_dir = $srv_config_dir;
    &mainloopServer;
}
else {
    &perr("Script is not run by FAI\n");
    &myexit(1);
}

if ( not $err ) {
    &plog("Finished: All fine\n");
}
&myexit($err);

################################3
# SERVER

sub mainloopServer {
    @target_files = &getReplaceFiles("");
    &printdb("Variable replacing for template files\n");
	
    for $filename (@target_files) {
        $template_file    = &getTmplName($filename);
        $post_repl_script = &getPostReplScript($filename);
        $tmpl_tmp         = "$template_file.$tmp_file_extension";
		
        $replaces = &replace( $template_file, $class );
        
		## not hing to be replaced
		if ( $replaces == 0 ) {
            &deleteFile($tmpl_tmp);
			return;
        }
        elsif( -e $post_repl_script){
            &replace( $post_repl_script, $class );
            &moveFile( "$post_repl_script.$tmp_file_extension",
                "$post_repl_script" );

			if ( &postScripts( $post_repl_script, $tmpl_tmp, $filename ) == 0 )
			{
				&plog("Post script successfully executed\n");
				&plog("Processing of $filename successfull\n\n");
			}
			else {
				&perr("Post replace unsuccessful\n");
				$err = $true;
			}
        }else{
			&plog("No post replace script found");
		}
		
		if ( &checkSuccess($tmpl_tmp) != 0 ) {
			&moveFile( $tmpl_tmp, "$filename" );
			&printdb("All variables successfully replaced\n");
		}
		else {
			&perr("Still unreplaced strings in templ\n");
			&deleteFile($tmpl_tmp) if not $keep_tmp_files;
		}
    }
}

sub existsToChar {
    if ( -e $_[0] ) {
        return "(exists)";
    }
    return "(does not exists)";
}

sub postScripts {
    my $post_repl_script = $_[0];
    my $tmpfile          = $_[1];
    my $filename         = $_[2];
    if ( -e $post_repl_script ) {
        &plog( "Executing replace script: " . $post_repl_script . "\n" );
            if ( system("$post_repl_script $tmpfile") == 0 ) {
                return 0;
            }
            else {
                return 1;
            }
    }
    else {
        return 0;
    }
}

##############################333

################ SUBROUTINES #########################
sub mainloopFAI {
    my @fai_classes = &getFaiClasses;
    my $nada_tmpl   = 1;
    my $replaces    = 0;
    my $success     = 0;
    for $class (@fai_classes) {

        &printdb("\nTreating FAI class $class\n");

        if ( &sourceHashes($class) == 0 ) {
            my @replace_files = &getReplaceFiles($class);
            my $size          = @replace_files;
            if ( $size != 0 ) {
                for $file (@replace_files) {
                    $replaces = &replace( "$file", $class );
                    $success = &checkSuccess("$file.$tmp_file_extension");
                    if ( $success != 0 ) {
                        if ( $replaces != 0 ) {
                            &moveFile( "$file.$tmp_file_extension", "$file" );
                            &printdb("All variables successfully replaced\n");
                        }
                        else {
			    &deleteFile( "$file.$tmp_file_extension");
                            &printdb("There was nothing to replace\n");
                        }
                    }
                    else {
                        &perr(
"Still unreplaced strings in template $file.$tmp_file_extension, leaving file in original status\n"
                        );
                        &perr(
"Keeping temporary file ($file.$tmp_file_extension) for inspection."
                        );
                    }
                }
                $nada_tmpl = 0;
            }
        }
        else {
            &printdb("Class $class has no rpl file\n");
        }
    }
    if ( $nada_tmpl == 1 ) {
        &perr("No template files at all!\n");
    }
}

# determine if this script is run by FAI
sub runByFai {
    if ( $ENV{'FAI_ACTION'} eq "install" || $ENV{'FAI_ACTION'} eq "softupdate" )
    {
        $fai_action = $ENV{'FAI_ACTION'};
        return 0;
    }
    else {
        return 1;
    }
}

# get defined FAI classes using the according environment variable
sub getFaiClasses {
    my @arr_classes = split( $fai_classes_delimiter, $classes );
    my $arr_size = @arr_classes;
    if ( $arr_size == 0 ) {
        &perr("No FAI classes defined\n");
        &myexit(1);
    }
    if ($debug) {
        &plog("FAI classes found: ");
        for $class (@arr_classes) {
            &plog("$class ");
        }
        &plog("\n");
    }

    return @arr_classes;
}

# loads the replace definitions into global hashes
sub sourceHashes {
    &printdb(
"Importing class hashes from file $replace_dir/$_[0].$replace_extension\n"
    );

    #	eval{
    #		eval ("%h = %$_[0]");
    #	};
    #	if (not $@){
    #		return 0;
    #	}

    if ( -e "$replace_dir/$_[0].$replace_extension" ) {
        require "$replace_dir/$_[0].$replace_extension";
        my %h = &getHashForClass( $_[0] );
        if ( exists $h{$require_array_name} ) {
            my @requirements = $h{$require_array_name};
            &printdb("Sourcing dependencies\n");
            for $req (@requirements) {
                &sourceHashes($req);
            }
            require "$replace_dir/$_[0].$replace_extension";
        }
    }
    else {
        return 0;
    }

    %obj = &getHashForClass( $_[0] );
    if ( keys(%obj) == 0 ) {
        &plog("No hashes defined for class $_[0]\n");
    }

    if ($debug) {
        while ( my ( $key, $value ) = each(%obj) ) {

            # do not print the name of the template files
            if ( $key ne $replace_array_name ) {
                &plog("  $key:$value\n");
            }
        }
    }
    return 0;
}

# gets a hash handle to deal with for a class
sub getHashForClass {
    my %h = ();
    eval "%h = %$_[0]";
    return %h;
}

# retrieves the files which should be replaced
# supports automode and normal mode
sub getReplaceFiles {
    my @replace_files;
    if ( $automode && not $run_by_server ) {
        @replace_files = &getReplaceFiles_automatic( $_[0] );
    }
    elsif ($run_by_server ) {
        @replace_files = &getReplaceFilesServer( $_[0] );
    }
    else {
        @replace_files = &getReplaceFiles_variable( $_[0] );
    }

    if ( @replace_files eq "" or @replace_files == () ) {
        &plog("No template files defined for $_[0]\n");
    }
    if ( $debug and scalar(@replace_files) != 0 and not $run_by_server ) {
        &plog("Following template files are defined for the class $_[0]:\n");
    }
    my @files_that_exist;
    my $full_path;
    foreach $file (@replace_files) {
        if ( $pmode eq "files" ) {
            $full_path = &normalizePath("$target/$file");
        }
        elsif ( $pmode eq "scripts" or $pmode eq "hooks" ) {
            $full_path = $file;
        }

        if ( not -e $full_path ) {
            &perr("Template file $full_path does not exist\n");
        }
        else {
	    &plog("$full_path\n");
            push( @files_that_exist, $full_path );
        }
    }

    return @files_that_exist;
}

sub getReplaceFilesServer {
    my $files;

    $files =
      &myexec("find $srv_file_path -type f \\( \! -regex \'.*/\\..*\' \\)");
    my @arr_files = split( /\n/, $files );
    return @arr_files;
}

# this function searches for file belonging to this
# class in the /files subdirectory of the FAI
# config space
sub getReplaceFiles_automatic {
    my @replace_files;
    my $files;
    if ( $pmode eq "scripts" ) {
        if ( -e "$fai_script_dir/$_[0]" ) {
            $files = &myexec("find $fai_script_dir/$_[0] -maxdepth 1 -type f");
        }

    }
    elsif ( $pmode eq "hooks" ) {
        $files =
          &myexec("find $fai_hooks_dir/ -maxdepth 1 -type f -name *$_[0]*");
    }
    else {
        my $exclude_string;
        for $exclude (@fai_replace_exclude_extensions) {
            $exclude_string ="$exclude_string -not -iwholename *.$exclude/$_[0]";
        }
        $files =
          &myexec(
"find $fai_files_dir -name $_[0] -size -$fai_max_tmpl_size $exclude_string"
          );
    }
    my @arr_files = split( /\n/, $files );
    for $file (@arr_files) {
        if ( $pmode eq "files" ) {
            my $realFile = &templatePathToRealPath( $file, $_[0] );
            my $fileType = &myexec("file -i -b $file");
            if ( $file =~ m/$filetype/ ) {
                push( @replace_files, $realFile );
            }
        }
        else {
            push( @replace_files, $file );
        }
    }

    return @replace_files;
}

# loads files which should be replaced for class
sub getReplaceFiles_variable {
    my %h = &getHashForClass( $_[0] );
    @replace_files = @{ $h{$replace_array_name} };
    return @replace_files;
}


sub getReplacement {
    my $class    = shift;
    my $variable = shift;

    my %h = &getHashForClass($class);
    if ( not exists $h{$variable} ) {
        &plog(
"Replacement for $class.$variable in template is not defined in $replace_dir/$class.$replace_extension\n"
        );
    }

    my $replacement = $h{$variable};
    &printdb("Getting replacement for $class.$variable = $replacement\n");

    return $replacement;
}

# opens the template file, replaces the template variables
# and writes the result to a temporary file
sub replace {
    open( REPL_FILE, $_[0] );
    open( TMP_FILE,  ">$_[0].$tmp_file_extension" );

    &printdb("\nOpened template file $_[0] for replacement\n");

    my $numReplacements = 0;
    while (<REPL_FILE>) {
        chomp;
        my $line = $_;
        my $interesting;
        while ( $line =~ m/(\[%\s*[a-zA-Z0-9\.\_]*\s*%\])/g ) {
            my ( $class, $classwar ) = &getClassAndVar($1, $_[1]);
            if ($run_by_server) {
                &sourceHashes($class);
            }

            my $match = $1;
            $numReplacements++;
            &printdb("\nMatched: $match processed to $class $classwar\n");

            $replacement = &getReplacement( $class, $classwar );
            $line =~ s/\Q$match\E/$replacement/;

            &printdb("Replacement: $line\n\n");

        }
        print TMP_FILE "$line\n";
    }

    &plog(
"$numReplacements items replaced in template file $_[0] of class $class\n"
    );

    close(TMP_FILE);
    close(REPL_FILE);
    return $numReplacements;
}

# gets a template string like [% UBUNTU.version %] as first 
# argument and makes
# an array like @arr = ("UBUNTU", "version") 
# out of it
# second argument is the default class for the case that
# the string is in the form [% version %] and the class
# can be derived from the file's path
sub getClassAndVar{
    my $string = $_[0];
    my $dclass = $_[1];
    $string =~ s/[\s+\[\]\%]//g;
    my @arr = split( /\./, $string );
    my $length = @arr;
    if ( $length == 1 ) {
        return ( $dclass, $arr[0] );
    }
    return ( $arr[0], $arr[1] );
}

### file operations ###
sub deleteFile {
    $file = $_[0];
    if ( -e $file ) {
        unlink($file);
        &printdb("Deleting $file\n");
    }
}

# moves or copies tmp file to the target file
# the new file will still have the same permissions
# as the original file
sub moveFile {
    $src = $_[0];
    $tar = $_[1];
	my $mode = ( stat $tar )->mode or die "no file $!";
	$mode = oct( sprintf "%04o", $mode & 07777 );
	if ($keep_tmp_files) {
		copy( $src, $tar ) or die "copy failed: $!";
		&printdb("Copied $src to $tar\n");
	}
	else {
		move( $src, $tar ) or die "move failed: $!";
		&printdb("Moved $src to $tar\n");
	}

	chmod( $mode, $tar );
}



### path routines #### 
# removes multiple slashes from path
sub normalizePath {
    my $path = $_[0];
    $path =~ s/\/+/\//g;
    return $path;
}

sub dissectPathAndFilename {
    my $orig   = $_[0];
    my @sp     = split( '/', $orig );
    my $length = @sp;
    my $filen  = @sp[-1];
    my $path   = join( "/", @sp[ 0 .. ( ($length) - 2 ) ] );
    return $path, $filen;
}

sub templatePathToRealPath {
    my $tmplPath = $_[0];
    $tmplPath =~ s/$fai_files_dir//;

    # replace the classname only at the end
    $tmplPath =~ s/$_[1]$//;
    chop($tmplPath);
    $tmplPath = &normalizePath($tmplPath);
    return $tmplPath;
}

sub getTmplName {
    my $path, $filen;
    $path, $filen = &dissectPathAndFilename( $_[0] );
	my $template_file;

    if ($srv_tmpl_are_hidden) {
        $template_file= "$path/.$filen.$srv_tmpl_ext";
    }
    else {
        $template_file= "$path/$filen.$srv_tmpl_ext";
    }
	
	&printdb( "Template File:      \t$template_file "
		. &existsToChar($template_file)
		. "\n" );
	return $template_file
}


sub getTmpName {
    my $path, $filen;
    $path, $filen = &dissectPathAndFilename( $_[0] );
    if ($tmp_files_are_hidden) {
        return "$path/.$filen.$tmp_file_extension";
    }
    else {
        return "$path/$filen.$tmp_file_extension";
    }
}

sub getPostReplScript {
    my $file  = $_[0];
    my @sp    = split( '/', $file );
    my $filen = @sp[-1];
    my $post  = "$post_scripts_path$filen.$post_scripts_extension";
	&printdb( "Post replace script:\t$post_repl_script "
		  . &existsToChar($post)
		  . "\n" );
    return $post;
}

### log routines ### 

sub printdb {
    if ($logfile) {
        print LOGFILE "$_[0]";
    }
    elsif ($debug) {
        print "$_[0]";
    }
}

sub plog {
    if ($logfile) {
        print LOGFILE "$_[0]";
    }
    else {
        print "$_[0]";
    }
}

sub perr {
    if ($logfile) {
        print LOGFILE "$_[0]";
        print STDERR "$_[0]";
    }
    else {
        print STDERR "$_[0]";
    }
}
### misc routines ####
sub checkSuccess {
    my $file = $_[0];
    my $cmd  = "grep $check_regexp $file";
    &printdb("Executing: $cmd\n");
    my $out    = `$cmd`;
    my $status = $? >> 8;
    &printdb("Exit Status: $status\n");
    return $status;
}

sub myexit {
    close(LOGFILE);
    exit( $_[0] );
}

sub myexec {
    &printdb( "Executing: $_[0]\n" );
    return `$_[0]`;
}

sub usage {
    print << "EOF";

Usage: fai-replace [OPTION] ... -m [server|hooks|scripts|files] ...

   -m [server|hooks|scripts|files]	Processing mode
   --processing-mode=[server|...]	Same as above

-- Optional Paramaeters
   -c class[.class]     Define classes.
   -t target_dir        Copy files relativ to target_dir.
   -s path,--fai-config-sapce path	FAI config space
   -a,--automode 			Automode
   -l logfile, --logfile=logfile	Logfile

   --max-template-size  		Maximum file size to be considerated for replacing
   --exclude-extensions=gif,jpg[,etc]   Exclude files of this type from replacing
   
   -v,--verbose				Create verbose output.
   -D,--debug				Create debug output.
   -h,--help				Print this help
-- SERVER MODE OPTIONS --
   --server-config-directory		Directory where the *.rpl files are
   --server-target-path			Directory where the templates are
   --server-post-scripts-path		Directory where the post processing scripts are
EOF
}


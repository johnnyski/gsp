#!/usr/bin/perl
MAIN:
{
	$nargs = 4-1;
	if ($#ARGV < $nargs) {
        print STDERR "Usage:\n $0 DIRECTORY SITEID YEAR VERBOSE\n";
		print STDERR "DIRECTORY above the data/ directory\n";
		print STDERR "SITEID is arbitrary site identificatio\n";
		print STDERR "YEAR beginning year of data\n";
		print STDERR "VERBOSE is the verbose flag.\n";
        exit;
	}
#
# ### Set defaults
#
	$prog     = "preproc";		# Program name
	$verbose  = "0";			# Verbose = on
	$net_id   = "???";			# Network ID
#
# ### Assign command line parameter values
#
	$dir      = shift;
	$net_id   = shift;
	$year     = shift;
	$verbose  = shift;
	$in_dir   = $dir."/data/raw/".$net_id."/";    # Input directory
	$out_dir  = $dir."/data/gag/".$net_id."/";    # Output directory
	if($net_id eq "KSO"){
		$in_dir   = $dir."/data/raw/KSC/";    # Input directory
		$out_dir  = $dir."/data/gag/KSC/";    # Output directory#
	}

#
# ### Clean the output directories.
#
	if($verbose == 1) {
		print "Removing old GAG files from $out_dir\n";
	}
	$command = "rm $out_dir/*.gag";
	system($command);
#
# ### Print to screen
#
	if($verbose == 1) {
		printf("NETWORK_ID: %s\n", $net_id);
		printf("INPUT Directory: %s\n", $in_dir);
		printf("OUTPUT Directory: %s\n", $out_dir);
	}
#
# ### Get a list of all of the input (*.gag) files, write their
# ### names one per line in the $prog.i file.
#
	open(FILE,"> file.list"); 	# Create list of input filenames	
	open(LIST,"ls $in_dir |"); 	
	while(<LIST>) {
		chop;
		$i++;
		$raw_file = $in_dir."/".$_;
		$mod_file = $raw_file.".tmp";
		printf("RAWFILE == %s\n",$raw_file);
		printf("MODFILE == %s\n",$mod_file);
		#
		# Remove blank lines from the input file. We will 
		#
		open(RAWFILE,"cat $raw_file |");
		open(MODFILE,"> $mod_file");
		#
		# While not at EOF of raw file, and cat all non
		# blank lines to mod_file
		#
	 	while(<RAWFILE>) {
			chop;	# Remove \n from end of line
			$nlines++;
			#
			# *** If the line is not blank, write it out.
			#
			if($_ ne "") {
				#
				# *** Harris County data sometimes has an integer value for the
				# *** rainrate, especially if it is 0. This will crash har.f 
				# *** when it tries to read the data.
				# *** A similar problems occurs with the TAM data.
				#
				$reform = 0;
				
				#
				# *** Texas A&M Mesonet Data
				#
				if($net_id eq "TAM") {
					($f1, $f2, $f3, $f4, $f5, $f6) = split(",",$_);
					if($f1 eq "4") { # Tip record
						$f4 = $f4*1.;
						$f5 = $f5*1.;
						printf(MODFILE "%s,%s,%s,%.2f,%.2f\n", 
						 		  $f1, $f2, $f3, $f4, $f5);
					} else { # Normal record
						$f5 = $f5*1.;
						$f6 = $f6*1.;
						printf(MODFILE "%s,%s,%s,%s,%.2f,%.2f\n", 
						 		  $f1, $f2, $f3, $f4, $f5, $f6);
					}					
				    $reform = 1;
				}
				#
				# *** HARRIS COUNTY
				#
				if($net_id eq "HAR") {
					($date, $time, $rate) = split(" ",$_);
					printf(MODFILE "%s %s %-12.8f\n", 
						   $date, $time, $rate*1.0);
				    $reform = 1;
				} 
				if($reform == 0) {
					printf(MODFILE "$_\n");
				}

			}
		}
		close(RAWFILE);
		close(MODFILE);
		#
		# *** Copy the modified data over the raw data.
		#
		print("Copying modfile to raw file...\n");
		$command = "cp $mod_file $raw_file";
		system($command);

		$command = "rm $mod_file";
		system($command);
		print(FILE "$raw_file \n");
	}	
	close(LIST);			# 						
	close(FILE);
	$command = $prog." file.list ".$net_id." ".$year." ".$verbose;
	print $command."\n";
#
# ### Execute $prog using $prog.i in current directory.
#

	system($command);

}

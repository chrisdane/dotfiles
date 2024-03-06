# r

# run subroutine clock_init() from gen_modules_clock.F90 if restart or not based on 
# given namelist.config:&clockinit, namelist.config:&calendar and fesom.clock

rm(list=ls())

################# input ####################

f <- list()
if (F) {
    # namelist.config:&clockinit:
    f$timenew <- 0.0 
    f$daynew <- 1
    f$yearnew <- 2686
    # namelist.config:&calendar:
    f$include_fleapyear <- T 
    # fesom.clock:
    f$fesom.clock <- rbind(c(84600, 365, 2685), 
                           c(0.0,   1,   2686))
} else if (T) {
    # namelist.config:&clockinit:
    f$timenew <- 0.0 
    f$daynew <- 1
    f$yearnew <- 1958
    # namelist.config:&calendar:
    f$include_fleapyear <- F 
    # fesom.clock:
    f$fesom.clock <- rbind(c(0.0, 1, 1958), 
                           c(0.0, 1, 1958))
}


################# input end ####################

# constants
num_day_in_month <- rbind(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                          c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

# subroutine check_fleapyr 
check_fleapyr <- function(include_fleapyear, year) {

    if (!include_fleapyear) {
        return(0)
    } else {
        if (is.leap(year)) {
            return(1)
        } else {
            return(0)
        }
    }

} # subroutine check_fleapyr

# subroutine clock_init
clock_init <- function(timenew, daynew, yearnew, include_fleapyear, fesom.clock) {
    
    #! the model inialized at
    timestart=timenew
    daystart=daynew
    yearstart=yearnew
    seconds_from_yearstart=0

    # ! init clock for this run
    timeold <- fesom.clock[1,1]
    dayold <- fesom.clock[1,2]
    yearold <- fesom.clock[1,3]
    timenew <- fesom.clock[2,1]
    daynew <- fesom.clock[2,2]
    yearnew <- fesom.clock[2,3]
    if (daynew==0) daynew=1

    #! check if this is a restart or not
    cat("check if restart with\n",
        "   yearnew = ", yearnew, "\n",
        "   yearstart = ", yearstart, "\n",
        "   daynew = ", daynew, "\n",
        "   daystart = ", daystart, "\n",
        "   timenew = ", timenew, "\n",
        "   timestart = ", timestart, " ...\n\n", sep="")
    if (yearnew == yearstart && daynew == daystart && timenew == timestart) {
        cat("   yearnew = yearstart = ", yearnew, "\n",
            "   daynew = daystart = ", daynew, "\n",
            "   timenew = timestart = ", timenew, "\n",
            "   --> no restart\n\n", sep="")
        r_restart=F
        yearold=yearnew-1 #!required for checking if create new output files
    } else {
        if (yearnew != yearstart) cat("   yearnew = ", yearnew, " != yearstart = ", yearstart, " --> restart\n")
        if (daynew != daystart) cat("   daynew = ", daynew, " != daystart = ", daystart, " --> restart\n")
        if (timenew != timestart) cat("   timenew = ", timenew, " != timestart = ", timestart, " --> restart\n")
        r_restart=T
        cat("\n")
    }

    # ! year as character string 
    #write(cyearold,'(i4)') yearold
    #write(cyearnew,'(i4)') yearnew

    #! if restart model at beginning of a day, set timenew to be zero
    if (timenew==86400.) {
       timenew=0.0
       daynew=daynew+1
    }

    #! set timeold to be timenew, ready for initializing forcing fields,
    #! yearold should not be updated here, which is requird to open input files.
    timeold=timenew 
    dayold=daynew

    #! check fleap year
    fleapyear <- check_fleapyr(include_fleapyear, yearnew) # fleapyear = 0 or 1
    ndpyr=365+fleapyear

    # ! find month and dayinmonth at the new time step
    aux1=0
    for (i in 1:12) {
       aux2=aux1+num_day_in_month[fleapyear+1,i] # fleapyear = 0 or 1
       if (daynew>aux1 && daynew<=aux2) {
          month=i
          day_in_month=daynew-aux1
          break # exit
       }
       aux1=aux2
    } # for i in 1:12

    cat("clock initialized at time ", timenew, " ", daynew, " ", yearnew, "\n", sep="")
    if (r_restart) {
        cat("THIS RUN IS A RESTART RUN!\n")
    }

    return(list(year_start=yearnew, month_start=month, day_start=day_in_month, r_restart=r_restart))

} # subroutine clock_init

# print input
cat("\ninput:\n")
print(f)

# check clock_init
cat("\nrun clock_init() ...\n\n")
ci <- clock_init(timenew=f$timenew, 
                 daynew=f$daynew, 
                 yearnew=f$yearnew, 
                 include_fleapyear=f$include_fleapyear, 
                 fesom.clock=f$fesom.clock)
cat("\nfinished clock_init() result:\n")
print(ci)

cat("\nfinished\n")


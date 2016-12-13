# Student Scheduling and Grouping Algorithms
Example genetic algorithm for scheduling students and grouping them by diversity.

There are 3 batch files which can be run:
* makeGroups.bat - This demonstrates grouping students by diversity
* makeSchedule.bat - This demonstrates filling activities with students while fullfilling complex enrollment requirments and activity contraints
* runApi.bat - A simple API for calling the grouping and scheduling code

This is still a work in progress and currently only setup to run on Windows.  However! The code is completely cross platform.  

Before running the above scripts open the SchedulingAndGrouping.sln solution file in Visual Studio, restore all NuGet packages and rebuild.  

**Coming Soon**
* Create build file which will run on Windows or Linux
* Create shell scripts for execution under Linux
* Convert projects to use DotNetCore 

# Student Scheduling and Grouping Algorithms
Example genetic algorithm for scheduling students and grouping them by diversity.  A poster was presented on this work at the University of Arizona IT Summit 2016.  [View the poster.](https://github.com/kiwicayne/student-scheduling/blob/master/Group%20Diversity%20and%20Scheduling%20with%20Genetic%20Algorithms.pdf)

There are 3 batch files which can be run:
* makeGroups.bat - This demonstrates grouping students by diversity
* makeSchedule.bat - This demonstrates filling activities with students while fullfilling complex enrollment requirments and activity contraints
* runApi.bat - A simple API for calling the grouping and scheduling code

This is still a work in progress and currently only setup to run on Windows.  However, the code is completely cross platform.  

Before running the above scripts open the SchedulingAndGrouping.sln solution file in Visual Studio, restore all NuGet packages and rebuild.  

**Coming Soon**
* Create build file which will run on Windows or Linux

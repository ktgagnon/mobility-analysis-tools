# mobility-analysis-tools
SUMMARY:
CalcRevisiting.R quantifies recurrent points in a trajectory. It can be useful for any x-y coordinates, including real-world FPS locations. For each location, or x-y point in a trajectory, CalcRevisiting computes the number of other points in the trajectory that share the same location, as well as the average distance to each of these points and the standard deviation of the distance to each point. The radius argument is used to control a threshold for what it means to "share the same location". Next, all points that share the same location with a given x-y coordinate, CalcRevisiting.R determines whether the person was loitering in that area or had left that region (defined by the radius and given x-y coordinate) and then returned (i.e, revisiting).

DEPENDENCIES:
This function utlizes three additional user-defined functions: DistanceFrom.R, InRadius.R, and FindSequences.R.
  DistanceFrom.R simply calculates the euclidean distance between a fixed locations and all other locations in a trajectory up to some point defined with an input argument.
  InRadius.R determines all distances (calculated using DistanceFrom.R) that fall within a specified radius.
  FindSequences.R finds consecutive sequences in a trajectory. For example, if you wanted to identify when a person stopped with their location was being tracked, you would observe a series of repeated coordinates at various times throughout the trajectory. FindSequences.R can identify where each of these stops began and ended, so you can later determine how long they paused for, how many unique pauses occurred, etc. Here, FindSequences.R is used to identify unique places in the trajectory that share the same location with a given x-y point based on the output from InRadius.R.
  
HOW TO USE:
Load all four functions mentioned and then pass CalcRevisiting.R x and y coordinates, along with a specified radius, a starting and ending point along the trajectory to analyze. It's highly recommended that you make sure you have equal spacing/timing between your location samples in the x-y coordinates. It is also highly recommended that you know the units of your x-y coordinates (e.g., meters) in order to determine a theoretically meaningful radius. The starting and ending points allow you control over which portion of the trajectory you want to analyze.
  
BACKGROUND INFORMATION:
  I've used this algorithm to detect caution while a person is exploring a novel environment. More revisiting behavior during exploration positively correlates with one's self-reported spatial anxiety, the degree with which they avoid physically harmful situations, and negatively correlates with one's navigation performance in that same novel environment. From what I can tell, this algorithm is similar to Recurrence Quantification Analysis (RQA), but that's something to explore in the future (no pun intended).
  

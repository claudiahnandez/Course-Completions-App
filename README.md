# Course-Completions-App
I am publishing this app because when I first started working on shiny I had a very narrow perspective as to what it can do.Would it be
easier to create a website from scratch? Yes, however the amount of data I am manipulating is too big to make SQL do all the heavy lifting
so I used R to help with that. Bear in mind this app is huge. I was hired by a community college to create this app whichs sole purpose is
to help understand the trends in students completion of a course based on certain factors such as ethnicity, gender, age, ect... 

## The UI
So the UI has a lot of plots, A LOT of plots! Early on I think if you are new to Shiny to be very maticulate about using <i>column()</i> 
throughout to get a good structure because the screen size will change and adding <i>column()</i> to the UI side will help keep things 
consitent. If you look under theme I also had to alter the positions for minor movements, this is evident in the UI especially since I 
clumped up objects to classes. Some tabs are really similar in structure so I was able to clump them up nicely and apply changes to all
of them all at once. </br>
Also note that I personally did add a tutorial page within the actual app because the app was way to extensive. I will not be displaying 
tutorial because it is quite a lot of reading material. I also added a simple FAQ. Applying this was actually tough. Due to the amount of 
graphs I clump up I do not have sufficient leeway to allow for a sidebar because it takes away from the user experience, however I needed 
more pages in my app so I was forced to add them. The hack was in <i>sidebar()</i> make collapse=T and also just make the dashboard button 
dissapear by changing the color. The FAQ and the tutorial are still made avilable by a <i>dropdownMenuOutput</i> that takes you to the page
by adding some javascript and a function in helper_function.r called "get_noti()". I got the idea from <a href="https://stackoverflow.com/questions/35728623/linking-notification-to-tab-in-shinydashboard">here</a>.
These are two of the tabs in the app.

Enrollment Tab             | Equity Index Tab          
:-------------------------:|:-------------------------:
![](https://image.ibb.co/hCTxx9/Completion_App.jpg)  |  ![](https://image.ibb.co/hvURqU/Completion_App2.jpg)

## The Server Side
<img align="right" src="https://image.ibb.co/cTpFjp/Completion_App3.jpg" width="40%" />
The server side I will say is the most cumbersome of them all because I have to keep track of SO many things. Basically if you click almost
anything it has to be reactive. I learned the hard way with this app that it is much more easier to make functions create the graphs for
you, however by the time I decided to implement this it was too big of a project already and bugs would have taken an exorbent amount of 
time to find. I would recommend anyone doing a shiny app to look into <i>isolate()</i>, this stops the reactivity when you dont need it but
by default anytime you call input$xxxxx it is automatically reactive, this can cause wierd errors.  The Historical Comparison Tab (on 
right) is a perfect example of an extremly reactive tab, by this I mean, as soon as the user selects something there is a domino
effect on other objects.


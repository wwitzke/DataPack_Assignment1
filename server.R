library(shiny)
library(ggplot2)
wrongnessBins <<- data.frame( bins = c(-3,-2,-1,0,1,2,3), counts = c(0, 0, 0, 0, 0, 0, 0));
lifespanBins <<- data.frame( bins = c(0,10,20,30,40,50,60,70,80,90,100), counts = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));
wrongnessBinsTemp <<- wrongnessBins;
lifespanBinsTemp <<- lifespanBinsTemp;


shinyServer(
    function(input, output)
    {
        realLifemean <<- abs(rnorm(1, 35, 15));
        realLifesd <<- abs(rnorm(1, 10, 8));
        realLifespan <<- abs(rnorm(1, realLifemean, realLifesd));
        firstTry <<- TRUE;
        evalCurrent = 0;
        degreeOfWrongness = 1;
        output$evaluation = renderText(
        {
            if( input$evaluate > 0 )
            {
                isolate(
                {
                    magicEvalText = "";
                    textPicker = sample.int(4,1);
                    lastFirstTry <<- firstTry;
                    firstTry <<- FALSE;
                    if ( lastFirstTry )
                    {
                        lifeBin = floor((input$estimate+5)/10) + 1;
                        if (
                            lifeBin > 0 && lifeBin < 12 && input$estimate != 0
                        )
                        {
                            lifespanBins[lifeBin, "counts"] <<- lifespanBins[lifeBin, "counts"]+1;
                        }
                    }

                    if( !lastFirstTry )
                    {
                        if ( textPicker == 1 )
                        {
                            magicEvalText = "If you're going to cheat, you're going to have to try harder then that.";
                        }
                        else if ( textPicker == 2 )
                        {
                            magicEvalText = "Only one evaluation per customer. Thank you, come again.";
                        }
                        else if ( textPicker == 3 )
                        {
                            magicEvalText = "If you weren't serious with your first estimate, then you don't DESERVE a second evaluation.";
                        }
                        else
                        {
                            magicEvalText = "Okay, fine, you want a new evaluation? Here it is: Your estimate is wrong. Super wrong. In fact, you're going to die tomorrow. No, make that today. Yea, that's right, Mr. or Ms. I-didn't-like-my-first-evaluation. You're going to die today, and there is nothing you can do about it.";
                        }
                    }
                    else
                    if(
                       input$estimate < (realLifespan+15/2)
                       &&
                       input$estimate > (realLifespan-15/2)
                    )
                    {
                        degreeOfWrongness <<- 4;
                        if ( textPicker == 1 )
                        {
                            magicEvalText = "It must be difficult to be right all the time.";
                        }
                        else if ( textPicker == 2 )
                        {
                            magicEvalText = "A realistic estimate of how your lifestyle will eventually lead to your demise. Good job.";
                        }
                        else if ( textPicker == 3 )
                        {
                            magicEvalText = "Have you considered trying to make money off your amazing prescient superpowers?";
                        }
                        else
                        {
                            magicEvalText = "Even a stopped clock is right twice a day. . . Unless, of course, one or both of the hands have been removed.";
                        }
                    }
                    else if(
                       input$estimate < (realLifespan+15+15/2)
                       &&
                       input$estimate > (realLifespan+15/2)
                    )
                    {
                        degreeOfWrongness <<- 5;
                        if ( textPicker == 1 )
                        {
                            magicEvalText = "I feel like you haven't really considered just how many ways there are for you to die, for instance: joyness overload, natural causes, martyrdom, russian roulette, paint huffing . . . the list goes on and on.";
                        }
                        else if ( textPicker == 2 )
                        {
                            magicEvalText = "I bet you're a real glass-is-half-full kind of person, aren't you?";
                        }
                        else if ( textPicker == 3 )
                        {
                            magicEvalText = "I feel like you haven't really considered just how many ways there are for you to die, for instance: mayonnaise inhalation, going on wikipedia, dying in your sleep, boredom, hemlock . . . the list goes on and on.";
                        }
                        else
                        {
                            magicEvalText = "Okay, look, let's be realistic here. Both you and I know that was a generous estimate. Not to say too much, but I wouldn't bother putting money aside for retirement if I were you.";
                        }
                    }
                    else if(
                       input$estimate < (realLifespan+30+15/2)
                       &&
                       input$estimate > (realLifespan+15+15/2)
                    )
                    {
                        degreeOfWrongness <<- 6;
                        if ( textPicker == 1 )
                        {
                            magicEvalText = "I feel like you haven't really considered just how many ways there are for you to die, for instance: method acting, cyanide, getting your hand caught in the teeth of a combine harvester, ripping your own brain stem out, ninjas . . . the list goes on and on.";
                        }
                        else if ( textPicker == 2 )
                        {
                            magicEvalText = "Okay, so, I guess somemone needs to prepare to be disappointed.";
                        }
                        else if ( textPicker == 3 )
                        {
                            magicEvalText = "I feel like you haven't really considered just how many ways there are for you to die, for instance: being a hero, being covered in tuna and thrown into a pit of hungry kittens, the truth pole, catch-22, misfiring orbital ion cannon . . . the list goes on and on.";
                        }
                        else
                        {
                            magicEvalText = "Look, I don't want to disabuse you of your delusionally optimistic view of the future, but you do realize you are a human, right?";
                        }
                    }
                    else if(
                       input$estimate < (realLifespan-15/2)
                       &&
                       input$estimate > (realLifespan-15-15/2)
                    )
                    {
                        degreeOfWrongness <<- 3;
                        if ( textPicker == 1 )
                        {
                            magicEvalText = "Hello emo friend from the early 2000's. I just want you to know that doom and gloom have gone out of style, and you don't have to bring people down with your calamitous predictions about the future anymore. Let's try looking more on the bright side from now on, okay? Okay.";
                        }
                        else if ( textPicker == 2 )
                        {
                            magicEvalText = "I bet you're a real glass-is-half-empty kind of person, aren't you?";
                        }
                        else if ( textPicker == 3 )
                        {
                            magicEvalText = "So, in fact, no, the icy cold grip of death will not be coming for you as soon as you thought. However, you are going to die horribly, if that makes you feel any better.";
                        }
                        else
                        {
                            magicEvalText = "Okay. Okay, my mistake. I should have been more clear in the instructions. You should be estimating when you think you ARE going to die, not when you think you SHOULD die in, you know, a just and sane world. I'll tell you what, give it another go.";
                            firstTry <<- TRUE;
                        }
                    }
                    else if(
                       input$estimate < (realLifespan-15-15/2)
                       &&
                       input$estimate > (realLifespan-30-15/2)
                    )
                    {
                        degreeOfWrongness <<- 2;
                        if ( textPicker == 1 )
                        {
                            magicEvalText = "Pretend this is a funny joke about suicide. I can't actually think of one, so you're going to have to make due on your own. You know, it'd be about how this ridiculously low estimate was a cry for help or . . . something . . . I don't know.";
                        }
                        else if ( textPicker == 2 )
                        {
                            magicEvalText = "If you are this pessimistic about how much time you have left in the world, then WHAT THE HELL ARE YOU DOING WASTING YOUR TIME HERE?!?";
                        }
                        else if ( textPicker == 3 )
                        {
                            magicEvalText = "Good news! You're not going to die nearly as soon as you thought! Bad news . . . You're not going to die nearly as soon as you want.";
                        }
                        else
                        {
                            magicEvalText = "It must be hard to be you, carrying around that oppressive anxiety all the time, jumping at every shadow, worrying about what doom every milisecond will bring. Man, seems like death would be a relief for someone who has such a bleak view of the future to estimate such a short lifespan. Well, I have bad news for you. . .";
                        }
                    }
                    else
                    {
                        if ( input$estimate < (realLifespan-30-15/2) )
                        {
                            degreeOfWrongness <<- 1;
                        }
                        else
                        {
                            degreeOfWrongness <<- 7;
                        }
                        if ( textPicker == 1 )
                        {
                            magicEvalText = "Oh. My. GOD. We TOLD you to be serious in your estimate! We told you! It's in the instructions! But, noooooo, you just had to try and be funny. Well, guess what, you don't GET an evaluation. You think evaluations are free? You think they grow on trees? THEY DON'T.";
                        }
                        else if ( textPicker == 2 )
                        {
                            magicEvalText = "I think probably a syberian tiger hopped up on acid with both dysentary and a crippling addiction to clowns would have been able to provide a better estimate then that. I'd tell you to try again, but honestly, after that poor showing, I think we both know that's not going to do you any good.";
                        }
                        else if ( textPicker == 3 )
                        {
                            magicEvalText = "So, let's be real here. You don't actually know what's going on do you? Did you read the instructions? Do you know what site you're on? Do you know where you are right now? You're not suffering a stroke are you? Here, try saying this out loud: How now brown cow. Did you manage it? Do you need a doctor? You probably need a doctor. SOMEBODY CALL A DOCTOR!";
                        }
                        else
                        {
                            magicEvalText = "Rarely has there been an estimate so awful that it killed babies. That's a haiku. Also it describes what you put in that box to the left."
                        }
                    }
                    if ( input$estimate != 0 )
                    {
                        wrongnessBins[degreeOfWrongness, "counts"] <<-
                            wrongnessBins[degreeOfWrongness, "counts"] + 1;
                    }
                    magicEvalText;
                });
            }
        });
        output$lifespanHist = renderPlot(
        {
            if( input$evaluate > 0 )
            {
                isolate(
                {
                    if( lastFirstTry )
                    {
                        cacheEstimate <<- input$estimate;
                        lifespanBinsTemp <<- lifespanBins;
                        numLS = sum ( lifespanBinsTemp[ , "counts"] );
                        if ( numLS < 100 )
                        {
                            for( ii in numLS:100 )
                            {
                                index = sample.int(11,1);
                                lifespanBinsTemp[ index, "counts" ] <<-
                                    lifespanBinsTemp[ index, "counts" ] + 1;
                            }
                        }
                    }
                    g = ggplot( lifespanBinsTemp, aes( bins ) ) +
                        geom_histogram( aes( weight = counts, fill = ..count.. ), bins = 11, col = "black" ) +
                        xlab("Remaining Lifespan Prediction (in Years)") + ylab("Count") +
                        ggtitle("Everybody's Horrible Lifespan Predictions") +
                        scale_fill_continuous( name = "Count" ) +
                        geom_vline( aes( xintercept = cacheEstimate, color = "YOU"), size = 1) +
                        scale_color_manual( name = "", values = c( YOU = "red" ) );
                    g
                });
            }
        });
        output$wrongnessHist = renderPlot(
        {
            if( input$evaluate > 0 )
            {
                isolate(
                {
                    if( lastFirstTry )
                    {
                        cacheYouValue <<- (input$estimate-realLifespan)/15;
                        wrongnessBinsTemp <<- wrongnessBins;
                        numWrong = sum ( wrongnessBinsTemp[ , "counts"] );
                        if ( numWrong < 100 )
                        {
                            for( ii in numWrong:100 )
                            {
                                index = sample.int(7,1);
                                wrongnessBinsTemp[ index, "counts" ] <<-
                                    wrongnessBinsTemp[ index, "counts" ] + 1;
                            }
                        }
                    }
                    g = ggplot( wrongnessBinsTemp, aes( bins ) ) +
                        geom_histogram( aes( weight = counts, fill = ..count.. ), bins = 7, col = "black" ) +
                        xlab("Standard Deviations Off the Correct Lifespan") + ylab("Count") +
                        ggtitle("How Bad Everybody Is At Estimating Their Lifespan") +
                        scale_fill_continuous( name = "Count" ) +
                        geom_vline( aes( xintercept = cacheYouValue, color = "YOU"), size = 1) +
                        scale_color_manual( name = "", values = c( YOU = "red" ) );
                    g
                });
            }
        });
        output$comments = renderText(
        {
            if( input$evaluate > 0 )
            {
                isolate(
                {
                    magicCommentText = "";
                    if( degreeOfWrongness == 1 )
                    {
                        magicCommentText = "Not everyone is cut out to make reasonable estimates. Some people are more cut out to be dog waxers, or stage jumpers, or maybe pottery clowns. The point is, you don't have to feel bad about your horrific and crippling lack of numeric proficiency. There are still a lot of options open for you, and I am certain you will do well, probably as a construction critic or maybe a train spotter . . . Or maybe you just clicked the button without putting in an estimate like a big idiot.";
                    }
                    else if( degreeOfWrongness == 2 )
                    {
                        magicCommentText = "So, you have an overinflated opinion about how your lifestyle choices and, frankly, deathwish actually impact your life. That is, you like to think you are dangerous, but you most certainly are not. But as you can see, YOU ARE NOT ALONE. There are a good number of people in exactly your boat, and they, too, will live way, WAY longer than they are prepared to. I suggest that you seriously consider saving up for the future, or at the very least learn to survive on the streets, because whatever happens, you're going to be dealing with it for a long, long time to come.";
                    }
                    else if( degreeOfWrongness == 3 )
                    {
                        magicCommentText = "I am pleased to announce that, despite your shockingly bad grasp of numbers and how the world works, you are going to be alive longer than you thought! That's right, you will have from between several and many more years to contemplate your terrible life choices! Isn't that a hoot? Time to slow down, take it all in, and waste all those precious moments, because you've got plenty to spare.";
                    }
                    else if( degreeOfWrongness == 4 )
                    {
                        magicCommentText = "Nobody likes a knnow-it-all, and I feel like this is one of the things that you know more than anybody else. Your disturbingly accurate understanding of your lifespan can only indicate that you are depressingly realistic, and therefore so boring that people hate to be around you, or that you cheated, and nobody likes cheaters. Either way, you should be ashamed of yourself. Now, go away and think about what you've done.";
                    }
                    else if( degreeOfWrongness == 5 )
                    {
                        magicCommentText = "Your optimisn is just a general problem that you need to get over. You need to get it through your head that the world is a dark and depressing place, and things just don't work out like they do in the movies. I know you thought that you'd live a long, prosperous life. Well, get over it, 'cause it's not going to happen. But, look on the bright side! You won't have to worry about the imminent collapse of the social security system, right?";
                    }
                    else if ( degreeOfWrongness == 6 )
                    {
                        magicCommentText = "Man, I hate giving bad news, and now is no exception. I'm sure that you hear this all the time, but boy are you wrong. I'm going to try my best to console you about your imminent and untimely demise, but there's only going to be so much I can do. I mean, I'm only a computer program creating some text in a box. Hell, I'm not even sentient. In fact, YOU should be pitying ME. What kind of life do I lead? I'll give you a hint: It's not good. So, yea, just keep that in mind when you think about your petty, dumb problems like your imminent death. I only WISH I could die.";
                    }
                    else if ( degreeOfWrongness == 7 )
                    {
                        magicCommentText = "Was it a typo? A fundamental misunderstanding of the directions? An attempt to be funny? A profound and disturbing failure to grasp reality? Who knows? What is certain, though, is that your clearly incorrect overestimate will be recorded here for all eternity, a testiment to whatever it was that caused you to be so very, very wrong.";
                    }
                    magicCommentText;
                });
            }
        });
    }
)

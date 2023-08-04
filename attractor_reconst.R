#Attractor reconstruction slide figures
#Ruby Krasnow
#Last modified: Aug 4, 2023

library(tidyverse)
library(rEDM)

fig_for_slide <- complete_tidy_diff %>% filter(Species=="jonah", Region== 1, Stratum==1, Type=="catch")
fig_for_slide$lag <- lead(fig_for_slide$value)
ggplot(data=fig_for_slide, aes(x=value, y=lag))+geom_line()

EmbedDimension(dataFrame = fig_for_slide %>% select(value), pred="1 37", lib="1 37", columns="value", target="value")
PredictNonlinear(dataFrame = fig_for_slide %>% select(value), pred="1 37", lib="1 37", columns="value", target="value", E=2)


# From model_comparison.R ---------------------------------------------------
library(plotly)

#### CATCH
tsdf_plot <- tsdf %>% mutate(yrs=index(ts1))

# Average catch over time
ggplot(data = tsdf_plot, aes(x=yrs, y=value))+
  geom_line()+
  labs(x="Years (t)", y="Avg. catch (x)")+
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

tsdf_plot <- tsdf_plot %>% mutate(lag_val = lag(value), lag_val2 = lag(lag_val)) 
tsdf_plot <-tsdf_plot %>% rbind(tsdf_plot %>% slice(3))

# E = 2 plot
ggplot(data = tsdf_plot, aes(x=value, y=lag_val))+
  geom_path()+
  labs(x="X(t)", y="X(t-1)")+
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

fig_catch <- plot_ly(tsdf_plot, x = ~value, y = ~lag_val, z = ~lag_val2, type = 'scatter3d', mode = 'lines+markers',
               line = list(width = 6, color = ~c, colorscale = 'Viridis'),
                marker = list(size = 3.5, color = ~c, colorscale = 'Greens'))
fig_catch

#### WEIGHT
ts_wt <- ts(catchTidy_agg %>% filter(Species=="jonah", Type=="wt") %>% mutate(date=as.Date(date)) %>% arrange(date), frequency = 2, start=c(2001, 1))

ts_wt<- na.spline(ts_wt) #using na.spline produces better EDM results than linear interpolation via na.approx

tsdf_wt <- data.frame(ts_wt[,c("Season", "Year", "value", "date")]) %>%
  mutate(lag_val = lag(value), lag_val2 = lag(lag_val), yrs = index(ts1))

tsdf_wt <-tsdf_wt %>% rbind(tsdf_wt %>% slice(3))

fig_wt <- plot_ly(tsdf_wt, x = ~value, y = ~lag_val, z = ~lag_val2, type = 'scatter3d', mode = 'lines+markers',
                     line = list(width = 6, color = ~c, colorscale = 'Viridis'),
                     marker = list(size = 3.5, color = ~c, colorscale = 'Greens'))

fig_wt

tsdf_combined <- left_join(tsdf_plot, tsdf_wt, by=c("Season", "Year", "date"))

plot_ly(tsdf_combined2, x = ~value.x, y = ~lag_val.x, z = ~lag_val2.x, type = 'scatter3d', mode = 'lines+markers',
     #   line = list(width = 6, color = ~c, colorscale = 'Viridis'),
        line = list(width = 6, color = 'black'),
        marker = list(size = 3.5, color = ~c, colorscale = 'Greens'))%>% 
  add_trace(x= ~value.y, y=~lag_val.y, z= ~lag_val2.y,
            line = list(width = 6, color = 'black'))
         #   line = list(width = 6, color = ~c, colorscale = 'Viridis'))

tsdf_combined2 <- tsdf_combined %>% mutate(across((starts_with("val") | starts_with("lag")), 
                     ~ (.x - mean(.x, na.rm=TRUE))/ sd(.x, na.rm=TRUE)))

# FIGURE IMAGE
plot_ly(tsdf_combined2 %>% slice(1:44), 
        x = ~value.x, 
        y = ~lag_val.x, 
        z = ~lag_val2.x, 
        type = 'scatter3d', 
        mode = 'lines+markers',
        line = list(width = 6, color = ~c, colorscale = 'Viridis'),
        marker = list(size = 3.5, color = ~c, colorscale = 'Greens')) %>% 
  layout(scene = list(xaxis = list(title = 'x(t)', showticklabels=FALSE),
                      yaxis = list(title = 'x(t-1)', showticklabels=FALSE),
                      zaxis = list(title = 'x(t-2)', showticklabels=FALSE)),
         font=list(size=15)) %>% 
  add_trace(x= ~value.y, y=~lag_val.y, z= ~lag_val2.y,
  line = list(width = 6, color = ~c, colorscale = 'Viridis'))

                   
# Average weight over time
ggplot(data = tsdf_wt, aes(x=yrs, y=value))+
  geom_line()+
  labs(x="Years", y="Avg. wt")+
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))


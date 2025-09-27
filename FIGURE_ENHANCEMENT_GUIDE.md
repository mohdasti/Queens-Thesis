# Figure Enhancement Guide

This guide provides recommendations for improving the visual quality and consistency of figures in the Queens-Thesis project.

## Current Figures

### 1. Sleep Hypnogram (`Figures/Sleep_Hypnogram.png`)
- **Purpose**: Illustrates typical sleep stage progression
- **Enhancement**: Ensure high resolution (300 DPI) for publication
- **Recommendation**: Convert to vector format (SVG/PDF) for scalability

### 2. Marble Maze Images (`Figures/Brio_marble_maze.jpg`, `Figures/Brio_marble_maze_above.jpg`)
- **Purpose**: Shows the visuomotor task apparatus
- **Enhancement**: Improve lighting and contrast
- **Recommendation**: Add scale reference and consistent background

### 3. Timeline Diagram (`Figures/timeline.pdf`)
- **Purpose**: Study design timeline
- **Enhancement**: Ensure consistent formatting and clear labels
- **Recommendation**: Use consistent color scheme throughout

## R-Generated Figures

### 1. Performance Distribution Plots
```r
# Enhanced version with better aesthetics
ggplot(df_GMean, aes(x = Condition, y = GeometricMean, color = Condition)) +
  geom_point(size = 4, alpha = 0.7, position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, color = "black", size = 4) +
  stat_summary(fun.ymin = function(x) (mean(x) - sd(x)),
               fun.ymax = function(x) (mean(x) + sd(x)),
               geom = "errorbar", width = 0.1) +
  scale_color_manual(values = c("MED" = "#2E8B57", "NAP" = "#4682B4", "WAKE" = "#CD5C5C")) +
  theme_apa() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  labs(
    title = "Declarative Memory Performance Across Conditions",
    x = "Experimental Condition",
    y = "Geometric Mean Score"
  )
```

### 2. Bayesian Results Plots
```r
# Enhanced Bayesian plot with better formatting
ggplot(means, aes(x=Level, y=Median, group=1)) +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher), 
                  size = 1.2, color = "#2E8B57") +
  geom_point(size = 3, color = "#2E8B57") +
  ylab("G-Mean") +
  xlab("Condition") +
  theme_apa() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Bayesian Analysis: Declarative Memory Performance",
    subtitle = "Error bars show 90% credible intervals"
  )
```

### 3. Acquisition Curve Plot
```r
# Enhanced acquisition curve with better visualization
ggplot(Acquisition_curve_overall, aes(x=Trials, y=Sum_Scores, group=Condition)) +
  geom_line(aes(color=Condition), size = 1.2) +
  geom_point(aes(color=Condition), size = 2.5) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="transparent", fill = "orange", alpha=0.3, inherit.aes = FALSE) +
  scale_color_manual(values = c("MED" = "#2E8B57", "NAP" = "#4682B4", "WAKE" = "#CD5C5C"),
                     labels = c("Meditation", "Napping", "Wake")) +
  theme_apa() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Learning Acquisition Curve",
    subtitle = "Highlighted area shows retention trials",
    x = "Trial Blocks",
    y = "Average Score",
    color = "Condition"
  )
```

## Figure Quality Standards

### Resolution and Format
- **Minimum Resolution**: 300 DPI for publication
- **Preferred Formats**: PDF (vector), PNG (raster), SVG (vector)
- **File Naming**: Use descriptive names with version numbers

### Color Scheme
- **Primary Colors**: 
  - Meditation: #2E8B57 (Sea Green)
  - Napping: #4682B4 (Steel Blue)
  - Wake: #CD5C5C (Indian Red)
- **Accessibility**: Ensure colorblind-friendly palette
- **Consistency**: Use same colors throughout all figures

### Typography
- **Font**: Arial or Helvetica for consistency
- **Sizes**: 
  - Titles: 14pt bold
  - Axis labels: 12pt bold
  - Axis text: 10pt
  - Legends: 10pt

### Layout and Spacing
- **Margins**: Adequate white space around figures
- **Aspect Ratio**: 4:3 or 16:9 for consistency
- **Grid Lines**: Minimal and subtle
- **Background**: White or transparent

## Figure Captions

### Format
```markdown
**Figure X.** *Descriptive title.* Detailed description of what the figure shows, including key findings and statistical information. Error bars represent standard deviation unless otherwise noted.
```

### Content Requirements
- Clear, descriptive title
- Explanation of what is shown
- Key findings highlighted
- Statistical information included
- Error bar explanation
- Sample size information

## Implementation Checklist

### For Each Figure:
- [ ] High resolution (300+ DPI)
- [ ] Consistent color scheme
- [ ] Clear, readable labels
- [ ] Appropriate aspect ratio
- [ ] Descriptive caption
- [ ] Accessibility considerations
- [ ] Version control (numbered files)

### For R Plots:
- [ ] Use `theme_apa()` for consistency
- [ ] Set appropriate colors manually
- [ ] Include error bars where relevant
- [ ] Add meaningful titles and labels
- [ ] Save in multiple formats (PDF, PNG)
- [ ] Use `ggsave()` with appropriate settings

## Tools and Resources

### R Packages for Visualization
```r
# Core plotting
library(ggplot2)
library(papaja)  # For APA theme

# Enhanced aesthetics
library(ggthemes)
library(viridis)  # Colorblind-friendly colors
library(scales)   # Better axis formatting

# Saving figures
library(here)     # Better file paths
```

### Figure Saving Function
```r
# Custom function for saving figures
save_figure <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggsave(
    filename = here("Figures", paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  
  ggsave(
    filename = here("Figures", paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    bg = "white"
  )
}
```

## Quality Control

### Before Final Submission:
1. **Review all figures** for consistency
2. **Check resolution** and file sizes
3. **Verify color accessibility** using colorblind simulators
4. **Test readability** at different sizes
5. **Ensure captions** are complete and accurate
6. **Validate statistical information** in figures

### Common Issues to Avoid:
- Low resolution images
- Inconsistent color schemes
- Missing error bars
- Unclear labels or legends
- Inappropriate aspect ratios
- Missing or incomplete captions
- Colorblind-unfriendly palettes

## Additional Resources

- [ggplot2 Documentation](https://ggplot2.tidyverse.org/)
- [Colorblind-Friendly Palettes](https://colorbrewer2.org/)
- [APA Style Guidelines for Figures](https://apastyle.apa.org/style-grammar-guidelines/tables-figures)
- [R Graphics Cookbook](https://r-graphics.org/)

(uiop:define-package #:app/widgets/service-stats
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:local-time
                #:timestamp<
                #:adjust-timestamp
                #:parse-timestring
                #:+iso-8601-date-format+
                #:format-timestring)
  (:import-from #:common/utils
                #:encode-json)
  (:import-from #:str
                #:replace-all)
  (:import-from #:app/analytics
                #:get-deps-users
                #:get-user-growth))
(in-package #:app/widgets/service-stats)

(in-readtable pythonic-string-syntax)


(defwidget service-stats-widget ()
  ())


(defun make-service-stats-widget ()
  (make-instance 'service-stats-widget))


(defparameter *init-charts* """
<!-- Styles -->
<style>
#growth-chart {
  width: 100%;
  height: 200px;
}
#deps-chart {
  width: 100%;
  height: 200px;
}

</style>

<!-- Resources -->
<script src="https://cdn.amcharts.com/lib/5/index.js"></script>
<script src="https://cdn.amcharts.com/lib/5/percent.js"></script>
<script src="https://cdn.amcharts.com/lib/5/xy.js"></script>
<script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>

<!-- Chart code -->
<script>
am5.ready(function() {

/// ПЕРВЫЙ ЧАРТ

// Create root element
// https://www.amcharts.com/docs/v5/getting-started/#Root_element
var root = am5.Root.new("growth-chart");


// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);

root.dateFormatter.setAll({
  dateFormat: "yyyy",
  dateFields: ["valueX"]
});

var data = FIRST_DATA_LIST;

// Create chart
// https://www.amcharts.com/docs/v5/charts/xy-chart/
var chart = root.container.children.push(am5xy.XYChart.new(root, {
  focusable: true,
  panX: false,
  panY: false,
  pinchZoomX: false,
  layout: root.verticalLayout
}));

var easing = am5.ease.linear;


// Create axes
// https://www.amcharts.com/docs/v5/charts/xy-chart/axes/
var xRenderer = am5xy.AxisRendererX.new(root, {});
var xAxis = chart.xAxes.push(am5xy.CategoryAxis.new(root, {
  categoryField: "month",
  renderer: xRenderer,
  tooltip: am5.Tooltip.new(root, {})
}));

xRenderer.grid.template.setAll({
  location: 1
});

xAxis.data.setAll(data);

var yAxis = chart.yAxes.push(am5xy.ValueAxis.new(root, {
  min: 0,
  renderer: am5xy.AxisRendererY.new(root, {
    strokeOpacity: 0.1
  })
}));


// Add series
// https://www.amcharts.com/docs/v5/charts/xy-chart/series/

var legend = chart.children.push(am5.Legend.new(root, {
  centerX: am5.p50,
  x: am5.p50
}));

function makeSeries(name, fieldName) {
  var series = chart.series.push(am5xy.ColumnSeries.new(root, {
    name: name,
    stacked: true,
    xAxis: xAxis,
    yAxis: yAxis,
    valueYField: fieldName,
    categoryXField: "month"
  }));

  series.columns.template.setAll({
    tooltipText: "{name}, {categoryX}: {valueY}",
    tooltipY: am5.percent(10)
  });
  series.data.setAll(data);

  // Make stuff animate on load
  // https://www.amcharts.com/docs/v5/concepts/animations/
  series.appear();

  series.bullets.push(function() {
    return am5.Bullet.new(root, {
      sprite: am5.Label.new(root, {
        text: "{valueY}",
        fill: root.interfaceColors.get("alternativeText"),
        centerY: am5.p50,
        centerX: am5.p50,
        populateText: true
      })
    });
  });

  legend.data.push(series);
}

makeSeries("Старички", "old-users");
makeSeries("Новенькие", "new-users");

// Make stuff animate on load
// https://www.amcharts.com/docs/v5/concepts/animations/
chart.appear(1000, 100);


/// ВТОРОЙ ЧАРТ

// Create root element
// https://www.amcharts.com/docs/v5/getting-started/#Root_element
var root = am5.Root.new("deps-chart");


// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);


// Create chart
// https://www.amcharts.com/docs/v5/charts/percent-charts/pie-chart/
var chart = root.container.children.push(am5percent.PieChart.new(root, {
  layout: root.verticalLayout
}));


// Create series
// https://www.amcharts.com/docs/v5/charts/percent-charts/pie-chart/#Series
var series = chart.series.push(am5percent.PieSeries.new(root, {
  valueField: "value",
  categoryField: "category"
}));


// Set data
// https://www.amcharts.com/docs/v5/charts/percent-charts/pie-chart/#Setting_data
series.data.setAll(SECOND_DATA_LIST);


// Play initial series animation
// https://www.amcharts.com/docs/v5/concepts/animations/#Animation_of_series
series.appear(1000, 100);

}); // end am5.ready()
</script>
""")


(defmethod render ((widget service-stats-widget))
  (with-html
    (:div :class "flex gap-8"
          (:raw (str:replace-all
                 "FIRST_DATA_LIST"
                 (encode-json (get-user-growth))
                 (str:replace-all
                  "SECOND_DATA_LIST"
                  (encode-json (get-deps-users))
                  *init-charts*)))
          (:div :class "w-full growth"
                (:div :class "users-count"
                      (:h1 "Количество сотрудников")
                      (:div :id "growth-chart")))
          (:div :class "w-full user-diagram"
                (:h1 "Распределение сотрудников по отделам")
                (:div :id "deps-chart")))))


(defmethod get-dependencies ((widget service-stats-widget))
  nil)

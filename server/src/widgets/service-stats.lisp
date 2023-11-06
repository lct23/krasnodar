(uiop:define-package #:admin/widgets/service-stats
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
                #:format-timestring))
(in-package #:admin/widgets/service-stats)

(in-readtable pythonic-string-syntax)


(defwidget service-stats-widget ()
  ())


(defun make-service-stats-widget ()
  (make-instance 'service-stats-widget))


(defparameter *pie-chart* """
<!-- Styles -->
<style>
#chartdiv1 {
  width: 100%;
  height: 200px;
}
</style>

<!-- Resources -->
<script src="https://cdn.amcharts.com/lib/5/index.js"></script>
<script src="https://cdn.amcharts.com/lib/5/percent.js"></script>
<script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>

<!-- Chart code -->
<script>
am5.ready(function() {

// Create root element
// https://www.amcharts.com/docs/v5/getting-started/#Root_element
var root = am5.Root.new("chartdiv1");


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
series.data.setAll([
  { value: 10, category: "Услугодатели" },
  { value: 60, category: "Организаторы" },
  { value: 30, category: "И то\nи другое" }
]);


// Play initial series animation
// https://www.amcharts.com/docs/v5/concepts/animations/#Animation_of_series
series.appear(1000, 100);

}); // end am5.ready()
</script>

<!-- HTML -->
<div id="chartdiv1"></div>
""")


(defparameter *services-growth*
  """
<!-- Styles -->
<style>
#chartdiv2 {
  width: 100%;
  height: 200px;
}
</style>

<!-- Resources -->
<script src="https://cdn.amcharts.com/lib/5/index.js"></script>
<script src="https://cdn.amcharts.com/lib/5/xy.js"></script>
<script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>

<!-- Chart code -->
<script>
am5.ready(function() {

// Create root element
// https://www.amcharts.com/docs/v5/getting-started/#Root_element
var root = am5.Root.new("chartdiv2");


// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);

root.dateFormatter.setAll({
  dateFormat: "yyyy",
  dateFields: ["valueX"]
});

var data = DATA_LIST;


// Create chart
// https://www.amcharts.com/docs/v5/charts/xy-chart/
var chart = root.container.children.push(am5xy.XYChart.new(root, {
  focusable: true,
  panX: false,
  panY: false,
  pinchZoomX: false
}));

var easing = am5.ease.linear;


// Create axes
// https://www.amcharts.com/docs/v5/charts/xy-chart/axes/
var xAxis = chart.xAxes.push(am5xy.DateAxis.new(root, {
  maxDeviation: 0.1,
  groupData: false,
  baseInterval: {
    timeUnit: "day",
    count: 1
  },
  renderer: am5xy.AxisRendererX.new(root, {

  }),
  tooltip: am5.Tooltip.new(root, {})
}));

var yAxis = chart.yAxes.push(am5xy.ValueAxis.new(root, {
  maxDeviation: 0.2,
  renderer: am5xy.AxisRendererY.new(root, {})
}));


// Add series
// https://www.amcharts.com/docs/v5/charts/xy-chart/series/
var series = chart.series.push(am5xy.LineSeries.new(root, {
  minBulletDistance: 10,
  connect: false,
  xAxis: xAxis,
  yAxis: yAxis,
  valueYField: "value",
  valueXField: "date",
  tooltip: am5.Tooltip.new(root, {
    pointerOrientation: "horizontal",
    labelText: "{valueY}"
  })
}));

series.fills.template.setAll({
  fillOpacity: 0.2,
  visible: true
});

series.strokes.template.setAll({
  strokeWidth: 2
});


// Set up data processor to parse string dates
// https://www.amcharts.com/docs/v5/concepts/data/#Pre_processing_data
series.data.processor = am5.DataProcessor.new(root, {
  dateFormat: "yyyy-MM-dd",
  dateFields: ["date"]
});

series.data.setAll(data);

series.bullets.push(function() {
  var circle = am5.Circle.new(root, {
    radius: 4,
    fill: root.interfaceColors.get("background"),
    stroke: series.get("fill"),
    strokeWidth: 2
  })

  return am5.Bullet.new(root, {
    sprite: circle
  })
});


// Add cursor
// https://www.amcharts.com/docs/v5/charts/xy-chart/cursor/
var cursor = chart.set("cursor", am5xy.XYCursor.new(root, {
  xAxis: xAxis,
  behavior: "none"
}));
cursor.lineY.set("visible", false);

// add scrollbar
//chart.set("scrollbarX", am5.Scrollbar.new(root, {
//  orientation: "horizontal"
//}));


// Make stuff animate on load
// https://www.amcharts.com/docs/v5/concepts/animations/
chart.appear(1000, 100);

}); // end am5.ready()
</script>

<!-- HTML -->
<div id="chartdiv2"></div>
""")

(defparameter *events-growth*
  """
<!-- Styles -->
<style>
#chartdiv3 {
  width: 100%;
  height: 200px;
}
</style>

<!-- Resources -->
<script src="https://cdn.amcharts.com/lib/5/index.js"></script>
<script src="https://cdn.amcharts.com/lib/5/xy.js"></script>
<script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>

<!-- Chart code -->
<script>
am5.ready(function() {

// Create root element
// https://www.amcharts.com/docs/v5/getting-started/#Root_element
var root = am5.Root.new("chartdiv3");


// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);

root.dateFormatter.setAll({
  dateFormat: "yyyy",
  dateFields: ["valueX"]
});

var data = DATA_LIST;


// Create chart
// https://www.amcharts.com/docs/v5/charts/xy-chart/
var chart = root.container.children.push(am5xy.XYChart.new(root, {
  focusable: true,
  panX: false,
  panY: false,
  pinchZoomX: false
}));

var easing = am5.ease.linear;


// Create axes
// https://www.amcharts.com/docs/v5/charts/xy-chart/axes/
var xAxis = chart.xAxes.push(am5xy.DateAxis.new(root, {
  maxDeviation: 0.1,
  groupData: false,
  baseInterval: {
    timeUnit: "day",
    count: 1
  },
  renderer: am5xy.AxisRendererX.new(root, {

  }),
  tooltip: am5.Tooltip.new(root, {})
}));

var yAxis = chart.yAxes.push(am5xy.ValueAxis.new(root, {
  maxDeviation: 0.2,
  renderer: am5xy.AxisRendererY.new(root, {})
}));


// Add series
// https://www.amcharts.com/docs/v5/charts/xy-chart/series/
var series = chart.series.push(am5xy.LineSeries.new(root, {
  minBulletDistance: 10,
  connect: false,
  xAxis: xAxis,
  yAxis: yAxis,
  valueYField: "value",
  valueXField: "date",
  tooltip: am5.Tooltip.new(root, {
    pointerOrientation: "horizontal",
    labelText: "{valueY}"
  })
}));

series.fills.template.setAll({
  fillOpacity: 0.2,
  visible: true
});

series.strokes.template.setAll({
  strokeWidth: 2
});


// Set up data processor to parse string dates
// https://www.amcharts.com/docs/v5/concepts/data/#Pre_processing_data
series.data.processor = am5.DataProcessor.new(root, {
  dateFormat: "yyyy-MM-dd",
  dateFields: ["date"]
});

series.data.setAll(data);

series.bullets.push(function() {
  var circle = am5.Circle.new(root, {
    radius: 4,
    fill: root.interfaceColors.get("background"),
    stroke: series.get("fill"),
    strokeWidth: 2
  })

  return am5.Bullet.new(root, {
    sprite: circle
  })
});


// Add cursor
// https://www.amcharts.com/docs/v5/charts/xy-chart/cursor/
var cursor = chart.set("cursor", am5xy.XYCursor.new(root, {
  xAxis: xAxis,
  behavior: "none"
}));
cursor.lineY.set("visible", false);

// add scrollbar
//chart.set("scrollbarX", am5.Scrollbar.new(root, {
//  orientation: "horizontal"
//}));


// Make stuff animate on load
// https://www.amcharts.com/docs/v5/concepts/animations/
chart.appear(1000, 100);

}); // end am5.ready()
</script>

<!-- HTML -->
<div id="chartdiv3"></div>
""")

(defparameter *services-categories*
  """
<!-- Styles -->
<style>
#chartdiv4 {
  width: 100%;
  height: 600px;
}
</style>

<!-- Resources -->
<script src="https://cdn.amcharts.com/lib/5/index.js"></script>
<script src="https://cdn.amcharts.com/lib/5/xy.js"></script>
<script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>

<!-- Chart code -->
<script>
am5.ready(function() {

var data = [];
var value1 = 20;
var value2 = 200;
var value3 = 2000;

var names = [
  "Остальное",
  "Печать билетов",
  "Реклама и продвижение",
  "Уборка помещений",
  "Охрана",
  "Фото и видеосъёмка",
  "Еда и кейтеринг",
  "Доставка цветов",
  "Аренда помещения"
];

for (var i = 0; i < names.length; i++) {
  value1 += Math.round(
    Math.random() * value1 * 2
  );
  data.push({
    category: names[i],
    value1: value1
  });
}

// Create root element
// https://www.amcharts.com/docs/v5/getting-started/#Root_element
var root = am5.Root.new("chartdiv4");

// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);

// Create chart
// https://www.amcharts.com/docs/v5/charts/xy-chart/
var chart = root.container.children.push(
  am5xy.XYChart.new(root, {
    panX: false,
    panY: false,
    arrangeTooltips: false,
    pinchZoomY: false
  })
);

// make x axes stack
chart.bottomAxesContainer.set("layout", root.horizontalLayout);

// Create axes
// https://www.amcharts.com/docs/v5/charts/xy-chart/axes/
var yRenderer = am5xy.AxisRendererY.new(root, {
  minGridDistance: 25
});

yRenderer.labels.template.setAll({
  multiLocation: 0.5,
  location: 0.5,
  paddingRight: 15
});

yRenderer.grid.template.set("location", 0.5);

var yAxis = chart.yAxes.push(
  am5xy.CategoryAxis.new(root, {
    categoryField: "category",
    tooltip: am5.Tooltip.new(root, {}),
    renderer: yRenderer
  })
);

yAxis.data.setAll(data);

// Add series
// https://www.amcharts.com/docs/v5/charts/xy-chart/series/
function createSeries(field, margin, column) {
  var xRenderer = am5xy.AxisRendererX.new(root, {
    minGridDistance: 40
  });
  
  xRenderer.labels.template.setAll({
    rotation: -90,
    centerY: am5.p50
  });
  
  var xAxis = chart.xAxes.push(
    am5xy.ValueAxis.new(root, {
      renderer: xRenderer,
      tooltip: am5.Tooltip.new(root, {
        animationDuration: 0
      }),
      marginLeft: margin // this makes gap between axes
    })
  );

  var series;
  if (column) {
    series = chart.series.push(
      am5xy.ColumnSeries.new(root, {
        xAxis: xAxis,
        yAxis: yAxis,
        valueXField: field,
        categoryYField: "category",
        sequencedInterpolation: true,
        tooltip: am5.Tooltip.new(root, {
          pointerOrientation: "horizontal",
          labelText: "{valueX}"
        })
      })
    );
  } else {
    series = chart.series.push(
      am5xy.LineSeries.new(root, {
        xAxis: xAxis,
        yAxis: yAxis,
        valueXField: field,
        categoryYField: "category",
        sequencedInterpolation: true,
        tooltip: am5.Tooltip.new(root, {
          pointerOrientation: "horizontal",
          labelText: "{valueX}"
        })
      })
    );
  }

  if (!column) {
    series.bullets.push(function () {
      return am5.Bullet.new(root, {
        locationX: 1,
        locationY: 0.5,
        sprite: am5.Circle.new(root, {
          radius: 4,
          fill: series.get("fill")
        })
      });
    });
  }

  series.data.setAll(data);
  series.appear();

  return series;
}

createSeries("value1", 0, true);
//createSeries("value2", 30, false);
//createSeries("value3", 30, false);

// Add cursor
// https://www.amcharts.com/docs/v5/charts/xy-chart/cursor/
var cursor = chart.set("cursor", am5xy.XYCursor.new(root, {
  behavior: "none",
  yAxis: yAxis
}));

// Make stuff animate on load
// https://www.amcharts.com/docs/v5/concepts/animations/
chart.appear(1000, 100);

}); // end am5.ready()
</script>

<!-- HTML -->
<div id="chartdiv4"></div>
""")


(defparameter *events-categories*
  """
<!-- Styles -->
<style>
#chartdiv5 {
  width: 100%;
  height: 600px;
}
</style>

<!-- Resources -->
<script src="https://cdn.amcharts.com/lib/5/index.js"></script>
<script src="https://cdn.amcharts.com/lib/5/xy.js"></script>
<script src="https://cdn.amcharts.com/lib/5/themes/Animated.js"></script>

<!-- Chart code -->
<script>
am5.ready(function() {

var events_data = [];
var value1 = 20;
var value2 = 200;
var value3 = 2000;

var names = [
  "Остальное",
  "Хакатоны",
  "Выставки",
  "Конкурсы",
  "Турниры",
  "Ярмарки",
  "Кинофестивали",
  "Концерты",
  
];

for (var i = 0; i < names.length; i++) {
  value1 += Math.round(
    Math.random() * value1 * 0.5
  );
  events_data.push({
    category: names[i],
    value1: value1
  });
}

// Create root element
// https://www.amcharts.com/docs/v5/getting-started/#Root_element
var root = am5.Root.new("chartdiv5");

// Set themes
// https://www.amcharts.com/docs/v5/concepts/themes/
root.setThemes([
  am5themes_Animated.new(root)
]);

// Create chart
// https://www.amcharts.com/docs/v5/charts/xy-chart/
var chart = root.container.children.push(
  am5xy.XYChart.new(root, {
    panX: false,
    panY: false,
    arrangeTooltips: false,
    pinchZoomY: false
  })
);

// make x axes stack
chart.bottomAxesContainer.set("layout", root.horizontalLayout);

// Create axes
// https://www.amcharts.com/docs/v5/charts/xy-chart/axes/
var yRenderer = am5xy.AxisRendererY.new(root, {
  minGridDistance: 25
});

yRenderer.labels.template.setAll({
  multiLocation: 0.5,
  location: 0.5,
  paddingRight: 15
});

yRenderer.grid.template.set("location", 0.5);

var yAxis = chart.yAxes.push(
  am5xy.CategoryAxis.new(root, {
    categoryField: "category",
    tooltip: am5.Tooltip.new(root, {}),
    renderer: yRenderer
  })
);

yAxis.data.setAll(events_data);

// Add series
// https://www.amcharts.com/docs/v5/charts/xy-chart/series/
function createSeries(field, margin, column) {
  var xRenderer = am5xy.AxisRendererX.new(root, {
    minGridDistance: 40
  });
  
  xRenderer.labels.template.setAll({
    rotation: -90,
    centerY: am5.p50
  });
  
  var xAxis = chart.xAxes.push(
    am5xy.ValueAxis.new(root, {
      renderer: xRenderer,
      tooltip: am5.Tooltip.new(root, {
        animationDuration: 0
      }),
      marginLeft: margin // this makes gap between axes
    })
  );

  var series;
  if (column) {
    series = chart.series.push(
      am5xy.ColumnSeries.new(root, {
        xAxis: xAxis,
        yAxis: yAxis,
        valueXField: field,
        categoryYField: "category",
        sequencedInterpolation: true,
        tooltip: am5.Tooltip.new(root, {
          pointerOrientation: "horizontal",
          labelText: "{valueX}"
        })
      })
    );
  } else {
    series = chart.series.push(
      am5xy.LineSeries.new(root, {
        xAxis: xAxis,
        yAxis: yAxis,
        valueXField: field,
        categoryYField: "category",
        sequencedInterpolation: true,
        tooltip: am5.Tooltip.new(root, {
          pointerOrientation: "horizontal",
          labelText: "{valueX}"
        })
      })
    );
  }

  if (!column) {
    series.bullets.push(function () {
      return am5.Bullet.new(root, {
        locationX: 1,
        locationY: 0.5,
        sprite: am5.Circle.new(root, {
          radius: 4,
          fill: series.get("fill")
        })
      });
    });
  }

  series.data.setAll(events_data);
  series.appear();

  return series;
}

createSeries("value1", 0, true);
//createSeries("value2", 30, false);
//createSeries("value3", 30, false);

// Add cursor
// https://www.amcharts.com/docs/v5/charts/xy-chart/cursor/
var cursor = chart.set("cursor", am5xy.XYCursor.new(root, {
  behavior: "none",
  yAxis: yAxis
}));

// Make stuff animate on load
// https://www.amcharts.com/docs/v5/concepts/animations/
chart.appear(1000, 100);

}); // end am5.ready()
</script>

<!-- HTML -->
<div id="chartdiv5"></div>
""")

(defmethod render ((widget service-stats-widget))
  (let ((service-growth-data
          (loop with limit = (parse-timestring "2026-05-18")
                for date = (parse-timestring "2023-05-18")
                then (adjust-timestamp date
                       (:offset :day 1))
                for slope = 0 then (if (< (random 1.0) 0.02)
                                       (random 0.005)
                                       slope)
                for value = 10
                then (+ value
                        (* value
                           slope)) 

                while (timestamp< date limit)
                collect (dict "date" (format-timestring nil date
                                                        :format +iso-8601-date-format+)
                              "value" value)))
        (events-growth-data
          (loop with limit = (parse-timestring "2026-05-18")
                for date = (parse-timestring "2023-05-18")
                then (adjust-timestamp date
                       (:offset :day 1))
                for slope = 0 then (if (< (random 1.0) 0.02)
                                       (random 0.005)
                                       slope)
                for value = 100
                then (+ value
                        (* value
                           slope)) 

                while (timestamp< date limit)
                collect (dict "date" (format-timestring nil date
                                                        :format +iso-8601-date-format+)
                              "value" value))))
    (with-html
      (:div :class "first-row"
            (:div :class "user-diagram"
                  (:h1 "Количество пользователей")
                  (:raw *pie-chart*))
            (:div :class "growth"
                  (:div :class "service-count"
                        (:h1 "Количество услуг")
                        (:raw (str:replace-all "DATA_LIST"
                                               (common/utils:encode-json service-growth-data)
                                               *services-growth*)))
                  (:div :class "events-count"
                        (:h1 "Количество мероприятий")
                        (:raw (str:replace-all "DATA_LIST"
                                               (common/utils:encode-json events-growth-data)
                                               *events-growth*)))))
      (:div :class "second-row"
            (:div :class "service-types"
                  (:h1 "Самые востребованные услуги")
                  (:h2 "Количество заказов за май")
                  (:raw *services-categories*))
            (:div :class "event-types"
                  (:h1 "Самые частые мероприятия")
                  (:h2 "Количество мероприятий за май")
                  (:raw *events-categories*))))))


(defmethod get-dependencies ((widget service-stats-widget))
  (list*
   (reblocks-lass:make-dependency
     `(.service-stats-widget
       (h1
        :text-align center
        :font-size 1.2rem
        :font-weight bold)
       (h2
        :text-align center
        :font-size 1.0rem
        :font-weight normal)
       ((:or .first-row
             .second-row)
        :display flex
        :margin-bottom 2rem
        :gap 3rem
        :align-items center
        :justify-content space-between
        ((:or .user-diagram
              .growth
              .event-types
              .service-types)
         :min-width 45%
         :max-width 100%))))
   (call-next-method)))

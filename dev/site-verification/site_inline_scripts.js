=== INLINE SCRIPT 1 ===
(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':            new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],        j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=        'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);    })(window,document,'script','dataLayer','GTM-M6RBPKQ');

=== INLINE SCRIPT 2 ===

        function MultipleDaysCheck(that) {
            if (that.value === "43200" || that.value === "10080") {
            document.getElementById("erlang-multiple-days").style.display = "block";
            } else {
            document.getElementById("erlang-multiple-days").style.display = "none";
            }
            var interval = parseInt(that.value);
            if (interval > 60) {
                document.getElementById("erlang-longer-than-hour").style.display = "block";
            } else {
                document.getElementById("erlang-longer-than-hour").style.display = "none";
            }
        }
        

=== INLINE SCRIPT 3 ===


            document.getElementById('erlang-form-total-percent').value=100;
            window.sum = function sum()
            {
                var d0 = parseFloat( document.getElementById('percentCallsPerDay0').value || 0);
                var d1 = parseFloat( document.getElementById('percentCallsPerDay1').value || 0);
                var d2 = parseFloat( document.getElementById('percentCallsPerDay2').value || 0);
                var d3 = parseFloat( document.getElementById('percentCallsPerDay3').value || 0);
                var d4 = parseFloat( document.getElementById('percentCallsPerDay4').value || 0);
                var d5 = parseFloat( document.getElementById('percentCallsPerDay5').value || 0);
                var d6 = parseFloat( document.getElementById('percentCallsPerDay6').value || 0);

                var totalPercent = d0 + d1 + d2 + d3 + d4 +  d5 + d6 ;
                totalPercent = Math.round(totalPercent * 100) / 100;

                if (totalPercent !== 100 ) {document.getElementById('erlang-form-total-percent').style.background="red" ; }
                if (totalPercent === 100 ) {document.getElementById('erlang-form-total-percent').style.background="" ; }

                document.getElementById('erlang-form-total-percent').innerHTML = totalPercent;


            };

        

=== INLINE SCRIPT 4 ===

            document.addEventListener("DOMContentLoaded", function () {
                $('.advanced-options-button').on('click',function () {
                    var advancedOptions = $('.advanced-options');
                    var buttonText = $('.but-text');

                    if (advancedOptions.is(":visible")) {
                        advancedOptions.hide(600);
                        buttonText.html("Show Advanced Options");
                    } else {
                        advancedOptions.show(600);
                        buttonText.html("Hide Advanced Options");
                    }
                });
            });
        

=== INLINE SCRIPT 5 ===


        // Load the Visualization API and the coreChart package.
        google.charts.load('current', {'packages': ['corechart']});

        // Set a callback to run when the Google Visualization API is loaded.
        google.charts.setOnLoadCallback(drawChart);

        // Callback that creates and populates a data table, instantiates the pie chart, passes in the data and draws it.
        function drawChart() {

            // Create the data table.
            var data = new google.visualization.DataTable();
            data.addColumn('string', 'Average Handling Time (AHT) mm:ss');
            data.addColumn('number', 'Percent');
            data.addRows([
                [ ' 0:00 - 0:59 ',1.5 ],[ ' 1:00 - 1:59 ',4.4 ],[ ' 2:00 - 2:59 ',3.6 ],[ ' 3:00 - 3:59 ',6 ],[ ' 4:00 - 4:59 ',21.6 ],[ ' 5:00 - 5:59 ',9.9 ],[ ' 6:00 - 7:59 ',17.1 ],[ ' 8:00 - 9:59 ',12.4 ],[ ' 10:00 - 14:99 ',9.6 ],[ ' 15:00 - 19:99 ',10.1 ],[ ' 20:00+ ',3.8 ]            ]);
            // Set chart options
            var options = {
                'title': 'Distribution of Average Handling Time',
                colors: ['#0066cc'],
                legend: {position: 'none'},
                bar: {groupWidth: "85%"},
                hAxis: {
                    title: 'AHT (Minutes)',
                    maxTextLines: 2,
                    maxAlternation: 1, // maximum layers of labels (setting this higher than 1 allows labels to stack over/under each other)
                    minTextSpacing: 2, // minimum space in pixels between adjacent labels// maximum number of lines to wrap to
                    textStyle: {
                        fontSize: 10 // or the number you want
                    }
                },
                vAxis: {
                    title: 'Percentage'
                }
            };

            // Instantiate and draw our chart, passing in some options.
            var chart = new google.visualization.ColumnChart(document.getElementById('chart_div'));
            chart.draw(data, options);
        }

        document.addEventListener("DOMContentLoaded", function () {
            $(window).resize(function () {
                drawChart();
            });
        });
    



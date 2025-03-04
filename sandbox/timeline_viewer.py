import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import pandas as pd
import random

# --- Data Definition (Easily Extendable) ---
enrollment_data = [
    {"PersonalID": "654657", "InformationDate": "2019-10-14", "EntryDate": "2019-10-14", "ExitAdjust": "2099-09-09", "ProjectType": 4, "lh_prior_livingsituation": True, "LivingSituation": 420},
    {"PersonalID": "654657", "InformationDate": "2022-03-15", "EntryDate": "2019-10-14", "ExitAdjust": "2099-09-09", "ProjectType": 4, "lh_prior_livingsituation": True, "LivingSituation": 450},
    {"PersonalID": "349625", "InformationDate": "2021-12-20", "EntryDate": "2021-10-27", "ExitAdjust": "2022-04-13", "ProjectType": 6, "lh_prior_livingsituation": False, "LivingSituation": 150},
    {"PersonalID": "684918", "InformationDate": "2022-03-04", "EntryDate": "2021-07-01", "ExitAdjust": "2022-07-14", "ProjectType": 14, "lh_prior_livingsituation": True, "LivingSituation": 302},
    {"PersonalID": "686041", "InformationDate": "2022-01-08", "EntryDate": "2021-07-23", "ExitAdjust": "2022-03-07", "ProjectType": 14, "lh_prior_livingsituation": False, "LivingSituation": 350},
    {"PersonalID": "556533", "InformationDate": "2021-11-24", "EntryDate": "2021-10-22", "ExitAdjust": "2022-03-04", "ProjectType": 4, "lh_prior_livingsituation": False, "LivingSituation": 250},
    {"PersonalID": "693996", "InformationDate": "2022-01-13", "EntryDate": "2021-12-27", "ExitAdjust": "2022-05-13", "ProjectType": 6, "lh_prior_livingsituation": False, "LivingSituation": 120},
    {"PersonalID": "614071", "InformationDate": "2022-03-30", "EntryDate": "2022-01-19", "ExitAdjust": "2022-06-24", "ProjectType": 6, "lh_prior_livingsituation": False, "LivingSituation": 50},
    {"PersonalID": "701796", "InformationDate": "2022-06-09", "EntryDate": "2022-06-07", "ExitAdjust": "2099-09-09", "ProjectType": 14, "lh_prior_livingsituation": False, "LivingSituation": 180},
    {"PersonalID": "702055", "InformationDate": "2022-06-16", "EntryDate": "2022-06-10", "ExitAdjust": "2099-09-09", "ProjectType": 6, "lh_prior_livingsituation": False, "LivingSituation": 320},
    {"PersonalID": "677683", "InformationDate": "2022-07-12", "EntryDate": "2022-07-13", "ExitAdjust": "2099-09-09", "ProjectType": 4, "lh_prior_livingsituation": False, "LivingSituation": 410}
]

# --- Helper function to generate random LivingSituation if missing ---
def assign_living_situation_category(living_situation):
    """Categorize living situation according to the provided ranges"""
    if 400 <= living_situation <= 499:
        return "permanent"
    elif (100 <= living_situation <= 199) or living_situation == 302:
        return "homeless"
    elif 300 <= living_situation <= 399:
        return "temporary"
    elif 200 <= living_situation <= 299:
        return "institutional"
    else:
        return "other"

# --- Data Preprocessing ---
def preprocess_data(data):
    data_dict = {}
    for record in data:
        if record.get("ExitDate") == "2099-09-09":
            record["ExitDate"] = None

        # If LivingSituation is not already in the record, add it
        if "LivingSituation" not in record:
            record["LivingSituation"] = random.randint(1, 499)

        key = (record["PersonalID"], record.get("EntryDate"), record.get("ExitDate"), 
               record["ProjectType"], record["lh_prior_livingsituation"], record["LivingSituation"])
        if key not in data_dict:
            data_dict[key] = {**record}
            data_dict[key]["InformationDate"] = [record["InformationDate"]]
        else:
            data_dict[key]["InformationDate"].append(record["InformationDate"])

    # Convert back to list
    data = list(data_dict.values())

    for item in data:
        if "InformationDate" in item:
            item["InformationDate"] = [pd.to_datetime(date, errors='coerce') for date in item["InformationDate"]]
        if "EntryDate" in item:
            item["EntryDate"] = pd.to_datetime(item["EntryDate"], errors='coerce')
        if "ExitAdjust" in item:
            item["ExitAdjust"] = pd.to_datetime(item["ExitAdjust"], errors='coerce')
        
        # Add living situation category
        item["LivingSituationCategory"] = assign_living_situation_category(item["LivingSituation"])
    return data

enrollment_data = preprocess_data(enrollment_data)

# --- Dynamically Determine Periods ---
def generate_periods(report_start, report_end):
    """
    Generates periods for each month between report_start and report_end, inclusive.

    Args:
        report_start (str or datetime): Start date of the reporting period.
        report_end (str or datetime): End date of the reporting period.

    Returns:
        list: A list of period dictionaries, each containing 'start', 'end', 'color', and 'label'.
    """
    report_start = pd.to_datetime(report_start)
    report_end = pd.to_datetime(report_end)
    periods = []
    current_date = report_start

    while current_date <= report_end:
        start_date = pd.to_datetime(f"{current_date.year}-{current_date.month:02d}-01")
        end_date = start_date + pd.offsets.MonthEnd(0)

        if start_date >= report_start and end_date <= report_end: # Ensure periods are within the overall report range
            label = start_date.strftime("%b %Y")
            periods.append({"start": start_date, "end": end_date, "color": "gray", "label": label})

        # Move to the next month
        current_date += pd.DateOffset(months=1)

    full_period_start = report_start
    full_period_end = report_end
    periods.insert(0, {"start": full_period_start, "end": full_period_end, "color": "black", "label": "Full Period"})

    return periods

# --- Plotting Function ---
def plot_timelines(enrollment_data, periods, report_start="2021-10-01", report_end="2022-09-30", info_marker_height=0.2, min_separation=0.2):
    timeline_start = pd.to_datetime(report_start) - pd.DateOffset(months=4)
    timeline_end = pd.to_datetime(report_end) + pd.DateOffset(months=1)

    # Create a slightly larger figure to accommodate two legends
    fig, ax = plt.subplots(figsize=(16, 8))
    colors = ['orange', 'brown', 'red', 'magenta', 'blue', 'green', 'purple', 'gray', 'cyan', 'olive']
    entry_marker = 'o'
    
    # Define exit markers based on living situation categories
    exit_markers = {
        "permanent": '^',  # solid triangle for permanent
        "temporary": 's',  # square for temporary
        "homeless": 'o',   # circle for homeless
        "institutional": 'o',  # circle for institutional
        "other": 'o'       # empty circle for other
    }
    
    # Define marker styles (filled or not)
    marker_styles = {
        "permanent": {'markerfacecolor': None},    # filled
        "temporary": {'markerfacecolor': None},    # filled
        "homeless": {'markerfacecolor': None},     # filled
        "institutional": {'markerfacecolor': 'white'},  # empty
        "other": {'markerfacecolor': 'white'}      # empty
    }
    
    dot_marker = '.'

    y_offset = -0.5
    period_y_offsets = {}
    for period in periods:
        period_y_offsets[period["label"]] = y_offset
        y_offset -= 0.5

    # Plot Report Periods (with vertically offset EECR/LECR and dots)
    for period in periods:
        y = period_y_offsets[period["label"]]
        ax.plot([period["start"], period["end"]], [y, y], color=period["color"], linewidth=2, label=period["label"])

        # 60/90 day window
        ax.plot([period["start"] - pd.DateOffset(days=90), period["start"]], [y, y], color=period["color"], linewidth=5, alpha=0.3)
        
    enrollment_positions = []
    legend_labels = []
    
    # Define and initialize marker legend items outside the loop
    marker_legend_items = {
        "permanent": {"marker": '^', "style": {'markerfacecolor': None}, 
                      "label": "Permanent Housing (400-499)"},
        "temporary": {"marker": 's', "style": {'markerfacecolor': None}, 
                     "label": "Temporary Housing (300-399)"},
        "homeless": {"marker": 'o', "style": {'markerfacecolor': None}, 
                    "label": "Homeless including TH (100-199, 302)"},
        "institutional": {"marker": 'o', "style": {'markerfacecolor': 'white'}, 
                         "label": "Institutional (200-299)"},
        "other": {"marker": 'o', "style": {'markerfacecolor': 'white'}, 
                 "label": "Other (0-99)"}
    }

    for idx, d in enumerate(enrollment_data):
        entry_date = d["EntryDate"]
        exit_date = d["ExitAdjust"]
        info_date = d["InformationDate"][0]
        living_situation_category = d["LivingSituationCategory"]
        
        closest_period_label = None
        min_distance = float('inf')

        for period in periods:
            if info_date:
              distance = abs((info_date - period["start"]).days) 
            else:
              distance = abs((entry_date - period["start"]).days) + abs((exit_date - period["end"]).days if d["ExitAdjust"] else 0)
            if distance < min_distance:
                min_distance = distance
                closest_period_label = period["label"]

        base_y_position = period_y_offsets[closest_period_label]
        y_position = base_y_position + 0.15

        for existing_y in enrollment_positions:
            while abs(y_position - existing_y) < min_separation:
                y_position += min_separation

        enrollment_positions.append(y_position)
        
        # Plot the enrollment line
        color = colors[idx % len(colors)]
        ax.plot([entry_date, exit_date], [y_position, y_position], color=color)
        
        # Plot entry marker
        ax.plot(entry_date, y_position, color=color, marker=entry_marker)
        
        # Plot exit marker with appropriate shape based on living situation
        exit_marker = exit_markers[living_situation_category]
        marker_style = marker_styles[living_situation_category]
        
        # Create a marker with the right properties
        ax.plot(exit_date, y_position, color=color, marker=exit_marker, 
                markeredgecolor=color, **marker_style, markersize=8)

        # Shorter vertical lines for info_dates
        for info_date in d["InformationDate"]:
            ax.plot([info_date, info_date], [y_position - info_marker_height / 4, y_position + info_marker_height / 4],
                    color=color, linewidth=2)

        lh_status = "LH Prior" if d["lh_prior_livingsituation"] else "Not LH Prior"
        label_text = f'ID {d["PersonalID"]}, ProjType = {d["ProjectType"]}, {lh_status}'
        legend_labels.append((y_position, plt.Line2D([0], [0], color=color, lw=4), label_text))
    
    # Sort and create the enrollment legend
    legend_labels.sort(key=lambda x: x[0], reverse=True)
    handles, labels = zip(*[(entry[1], entry[2]) for entry in legend_labels])    
    
    # Create living situation marker legend
    marker_handles = []
    marker_labels = []
    
    for category, info in marker_legend_items.items():
        handle = plt.Line2D([0], [0], marker=info["marker"], color='black', 
                           markeredgecolor='black', **info["style"], 
                           linestyle='None', markersize=10)
        marker_handles.append(handle)
        marker_labels.append(info["label"])
    
    # Add legends to the plot
    # First legend: Enrollment information (ID, Project Type, LH Prior)
    enrollment_legend = ax.legend(handles, labels, loc='upper left', 
                                 bbox_to_anchor=(1.01, 1), title="Enrollments")
    
    # Add the enrollment legend as a separate artist
    ax.add_artist(enrollment_legend)
    
    # Second legend: Marker types (living situation categories)
    marker_legend = plt.legend(marker_handles, marker_labels, loc='upper left', 
                              bbox_to_anchor=(1.01, 0.5), title="Living Situation Categories")
    
    ax.set_xlim([timeline_start, timeline_end])
    ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
    ax.grid(axis='x', linestyle='--', alpha=0.3, color="black")
    plt.xticks(rotation=45)
    ax.set_xlabel("Date")
    ax.set_title("Enrollment Timelines with Living Situation Markers")
    ax.set_yticks([])
    plt.tight_layout()
    plt.show()

# --- Main Execution ---
report_start = pd.to_datetime("2021-10-01")
report_end = pd.to_datetime("2022-10-30")
periods = generate_periods(report_start, report_end)
plot_timelines(enrollment_data, periods, report_start, report_end)
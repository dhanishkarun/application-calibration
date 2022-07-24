﻿Imports System.ComponentModel

Public Class Form1
    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub DDMT_TextChanged(sender As Object, e As EventArgs) Handles DDMT.TextChanged

    End Sub

    Private Sub uAT_TextChanged(sender As Object, e As EventArgs) Handles uAT.TextChanged

    End Sub

    Private Sub Panel1_Paint(sender As Object, e As PaintEventArgs) Handles Panel1.Paint

    End Sub

    Private Sub CONVERT_Click(sender As Object, e As EventArgs) Handles CONVERT.Click
        Dim ddm As Double
        Dim ddmp As Double
        Dim uA As Double
        Dim degree As Double
        If ddmR.Checked AndAlso Double.TryParse(DDMT.Text, ddmp) = False Then
            MessageBox.Show("Can't convert the DDM % value !")
        ElseIf ddmpR.Checked AndAlso Double.TryParse(DDMPT.Text, ddm) = False Then
            MessageBox.Show("Can't convert the DDM value !")
        ElseIf uAR.Checked AndAlso Double.TryParse(uAT.Text, uA) = False Then
            MessageBox.Show("Can't convert the uA value !")
        ElseIf degreeR.Checked AndAlso Double.TryParse(DEGREET.Text, degree) = False Then
            MessageBox.Show("Can't convert the degree value !")

        Else
            If ddmR.Checked Then
                ddm = Val(DDMT.Text)
                ddmp = ddm * 100
                degree = ddm * 0.36 / 0.0875
                uA = ddm * 75 / 0.0875
                uAT.Text = uA.ToString("0.000")
                DEGREET.Text = degree.ToString("0.000")
                DDMPT.Text = ddmp.ToString("0.000")

            ElseIf ddmpR.Checked Then
                ddmp = Val(DDMPT.Text)
                ddm = ddmp / 100
                degree = ddm * 0.36 / 0.0875
                uA = ddm * 75 / 0.0875
                DDMT.Text = ddm.ToString("0.0000")
                uAT.Text = uA.ToString("0.000")
                DEGREET.Text = degree.ToString("0.000")

            ElseIf uAR.Checked Then
                uA = Val(uAT.Text)
                degree = uA * 0.36 / 75
                ddm = uA * 0.0875 / 75
                ddmp = uA * 0.0875 / 75 * 100
                DDMPT.Text = ddmp.ToString("0.00")
                DDMT.Text = ddm.ToString("0.0000")
                DEGREET.Text = degree.ToString("0.000")

            ElseIf degreeR.Checked Then
                degree = Val(DEGREET.Text)
                ddm = degree * 0.0875 / 0.36
                ddmp = ddm * 100
                uA = degree * 75 / 0.36
                uAT.Text = uA.ToString("0.000")
                DDMT.Text = ddm.ToString("0.0000")
                DDMPT.Text = ddmp.ToString("0.000")
            End If



        End If


    End Sub



    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim sbo_rf As Double
        Dim r_hsw As Double
        Dim m_hsw As Double
        Dim r_sborf As Double

        If Double.TryParse(sborf.Text, sbo_rf) = False Then
            MessageBox.Show("Can't convert the SBO RF value !")
        ElseIf Double.TryParse(rhsw.Text, r_hsw) = False Then
            MessageBox.Show("Can't convert the Reqd H.S.W Value !")
        ElseIf Double.TryParse(mhsw.Text, m_hsw) = False Then
            MessageBox.Show("Can't convert the Measured HSW value !")

        Else
            sbo_rf = Val(sborf.Text)
            r_hsw = Val(rhsw.Text)
            m_hsw = Val(mhsw.Text)

            r_sborf = sbo_rf + 20 * Math.Log10(m_hsw / r_hsw)

            rsborf.Text = r_sborf.ToString("0.00")


        End If
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'Dim fileReader As String
        'fileReader = My.Computer.FileSystem.ReadAllText("reading.txt")
        'Dim strArray() As String = fileReader.Split("|") 'change the | by your separator
        Tx1.Checked = True
        If Tx1.Checked Then
            sborf.Text = My.Settings.sborfdb1
            sborf1.Text = My.Settings.sborfdb1

            baseddm.Text = My.Settings.baseddm

            mbaseddm.Text = My.Settings.baseddm
            rhsw.Text = My.Settings.calibhsw1

            cwidthddm.Text = My.Settings.monwidread1

            monitor.Text = My.Settings.monwidread1

            Nsborf.Text = My.Settings.sborfdb1

        Else
            sborf.Text = My.Settings.sborfdb2
            sborf1.Text = My.Settings.sborfdb2

            baseddm.Text = My.Settings.baseddm

            mbaseddm.Text = My.Settings.baseddm
            rhsw.Text = My.Settings.calibhsw2

            cwidthddm.Text = My.Settings.monwidread2

            monitor.Text = My.Settings.monwidread2

            Nsborf.Text = My.Settings.sborfdb2
        End If
        limit.Text = My.Settings.limit
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim N_sborf As Double
        Dim mlimit As Double
        Dim mbase_ddm As Double
        Dim m_width As Double
        Dim wide_alarm As Double
        Dim wide_ddm As Double
        Dim narr_alarm As Double
        Dim narr_ddm As Double

        If Double.TryParse(Nsborf.Text, N_sborf) = False Then
            MessageBox.Show("Can't convert the CURRENT SBO RF value !")
        ElseIf Double.TryParse(limit.Text, mlimit) = False Then
            MessageBox.Show("Can't convert the Reqd ALARM LIMIT Value !")
        ElseIf Double.TryParse(mbaseddm.Text, mbase_ddm) = False Then
            MessageBox.Show("Can't convert the Reqd ALARM LIMIT Value !")
        ElseIf Double.TryParse(monitor.Text, m_width) = False Then
            MessageBox.Show("Can't convert the Measured Monitor Width value !")

        Else
            N_sborf = Val(Nsborf.Text)
            mlimit = Val(limit.Text)
            m_width = Val(monitor.Text)
            mbase_ddm = Val(mbaseddm.Text)
            wide_alarm = 20 * Math.Log10(1 - mlimit / 100)
            narr_alarm = 20 * Math.Log10(1 + mlimit / 100)
            wide_ddm = m_width + ((mbase_ddm + m_width) * (-1 * mlimit / 100))
            narr_ddm = m_width + ((mbase_ddm + m_width) * (1 * mlimit / 100))

            widealarm.Text = wide_alarm.ToString("0.00")
            narrowalarm.Text = narr_alarm.ToString("0.00")
            widemonitor.Text = wide_ddm.ToString("0.000")
            narrowmonitor.Text = narr_ddm.ToString("0.000")


        End If
    End Sub

    Private Sub Label19_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub Label20_Click(sender As Object, e As EventArgs)

    End Sub





    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim sborf_1 As Double
        Dim c_width_ddm As Double
        Dim r_width_ddm As Double
        Dim r_sborf1 As Double
        Dim base_ddm As Double

        If Double.TryParse(sborf1.Text, sborf_1) = False Then
            MessageBox.Show("Can't process the SBO RF % value !")
        ElseIf Double.TryParse(baseddm.Text, base_ddm) = False Then
            MessageBox.Show("Can't process the BASE DDM value !")
        ElseIf Double.TryParse(cwidthddm.Text, c_width_ddm) = False Then
            MessageBox.Show("Can't process the Current WIDTH DDM Value !")
        ElseIf Double.TryParse(rwidthddm.Text, r_width_ddm) = False Then
            MessageBox.Show("Can't process the Required WIDTH DDM value !")

        Else
            sborf_1 = Val(sborf1.Text)
            c_width_ddm = Val(cwidthddm.Text)
            r_width_ddm = Val(rwidthddm.Text)
            base_ddm = Val(baseddm.Text)

            r_sborf1 = sborf_1 + 20 * Math.Log10((base_ddm + r_width_ddm) / (base_ddm + c_width_ddm))

            rsborf1.Text = r_sborf1.ToString("0.00")


        End If
    End Sub

    Private Sub Panel7_Paint(sender As Object, e As PaintEventArgs) Handles Panel7.Paint

    End Sub

    Private Sub monitor_TextChanged(sender As Object, e As EventArgs) Handles monitor.TextChanged

    End Sub

    Private Sub Form1_Closed(sender As Object, e As EventArgs) Handles Me.Closed

    End Sub

    Private Sub Form1_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing


    End Sub

    Private Sub save_Click(sender As Object, e As EventArgs) Handles save.Click
        If Tx1.Checked Then
            My.Settings.sborfdb1 = sborf.Text

            My.Settings.baseddm = mbaseddm.Text

            My.Settings.monwidread1 = monitor.Text
            My.Settings.calibhsw1 = rhsw.Text
        Else
            My.Settings.sborfdb2 = sborf.Text

            My.Settings.baseddm = mbaseddm.Text

            My.Settings.monwidread2 = monitor.Text
            My.Settings.calibhsw2 = rhsw.Text

        End If
        My.Settings.limit = limit.Text
        My.Settings.Save()
    End Sub

    Private Sub Tx1_CheckedChanged(sender As Object, e As EventArgs) Handles Tx1.CheckedChanged
        If Tx1.Checked Then
            sborf.Text = My.Settings.sborfdb1
            sborf1.Text = My.Settings.sborfdb1

            baseddm.Text = My.Settings.baseddm

            mbaseddm.Text = My.Settings.baseddm
            rhsw.Text = My.Settings.calibhsw1

            cwidthddm.Text = My.Settings.monwidread1

            monitor.Text = My.Settings.monwidread1

            Nsborf.Text = My.Settings.sborfdb1

        Else
            sborf.Text = My.Settings.sborfdb2
            sborf1.Text = My.Settings.sborfdb2

            baseddm.Text = My.Settings.baseddm

            mbaseddm.Text = My.Settings.baseddm
            rhsw.Text = My.Settings.calibhsw2

            cwidthddm.Text = My.Settings.monwidread2

            monitor.Text = My.Settings.monwidread2

            Nsborf.Text = My.Settings.sborfdb2
        End If
    End Sub
End Class
